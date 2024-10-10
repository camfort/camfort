{- |
Module      :  Camfort.Specification.Units.Analysis
Description :  Helpers for units refactoring and analysis.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Camfort.Specification.Units.Analysis
  ( UnitAnalysis
  , compileUnits
  , initInference
  , runInference
  , runUnitAnalysis
    -- ** Helpers
  , puName
  , puSrcName
  ) where

import           Camfort.Analysis
import           Camfort.Analysis.Annotations (Annotation)
import           Camfort.Analysis.CommentAnnotator (annotateComments)
import           Camfort.Analysis.Logger (LogLevel(..))
import           Camfort.Analysis.ModFile (withCombinedEnvironment)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.InferenceBackend
import qualified Camfort.Specification.Units.InferenceBackendFlint as Flint
import qualified Camfort.Specification.Units.InferenceBackendSBV as BackendSBV
import           Camfort.Specification.Units.ModFile
  (genUnitsModFile, initializeModFiles, runCompileUnits)
import           Camfort.Specification.Units.Monad
import           Camfort.Specification.Units.MonadTypes
import           Camfort.Specification.Units.Parser (unitParser)
import qualified Camfort.Specification.Units.Parser.Types as P
import           Control.Lens ((^?), _1)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Lazy
import qualified Data.Array as A
import           Data.Data (Data)
import           Data.Generics.Uniplate.Operations
import qualified Data.IntMap.Strict as IM
import           Data.List (nub, intercalate)
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust, fromMaybe, mapMaybe)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Language.Fortran.AST as F
import           Language.Fortran.Analysis (constExp, varName, srcName)
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.SemanticTypes as FAS
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import           Language.Fortran.AST.Literal.Real (readRealLit, parseRealLit)
import           Language.Fortran.Util.ModFile
import qualified Numeric.LinearAlgebra as H -- for debugging
import           Prelude hiding (mod)
import           Language.Fortran.Repr (fromConstReal, fromConstInt)

-- | Prepare to run an inference function.
initInference :: UnitSolver ()
initInference = do
  pf <- getProgramFile

  -- Parse unit annotations found in comments and link to their
  -- corresponding statements in the AST.
  let (linkedPF, _) =
        runWriter $ annotateComments unitParser
        (\srcSpan err -> tell $ "Error " ++ show srcSpan ++ ": " ++ show err) pf
  modifyProgramFile $ const linkedPF

  -- The following insert* functions examine the AST and insert
  -- mappings into the tables stored in the UnitState.

  -- First, find all given unit annotations and insert them into our
  -- mappings.  Also obtain all unit alias definitions.
  insertGivenUnits

  -- For function or subroutine parameters (or return variables) that
  -- are not given explicit units, give them a parametric polymorphic
  -- unit.
  insertParametricUnits

  -- Any other variables get assigned a unique undetermined unit named
  -- after the variable. This assumes that all variables have unique
  -- names, which the renaming module already has assured.
  insertUndeterminedUnits

  -- Now take the information that we have gathered and annotate the
  -- variable expressions within the AST with it.
  annotateAllVariables

  -- Annotate the literals within the program based upon the
  -- Literals-mode option.
  annotateLiterals

  -- With the variable expressions annotated, we now propagate the
  -- information throughout the AST, giving units to as many
  -- expressions as possible, and also constraints wherever
  -- appropriate.
  propagateUnits

  -- Gather up all of the constraints that we identified in the AST.
  -- These constraints will include parametric polymorphic units that
  -- have not yet been instantiated into their particular uses.
  abstractCons <- extractConstraints
  dumpConsM "***abstractCons" abstractCons

  -- Eliminate all parametric polymorphic units by copying them for
  -- each specific use cases and substituting a unique call-site
  -- identifier that distinguishes each use-case from the others.
  cons <- applyTemplates abstractCons
  dumpConsM "***concreteCons" cons

  -- Remove any traces of CommentAnnotator, since the annotations can
  -- cause generic operations traversing the AST to get confused.
  modifyProgramFile UA.cleanLinks

  modifyConstraints (const cons)

  debugLogging

-- | Run a 'UnitSolver' analysis within a 'UnitsAnalysis'.
runInference :: UnitSolver a -> UnitAnalysis (a, UnitState)
runInference solver = do
  pf <- asks unitProgramFile
  mfs <- lift analysisModFiles

  let (pf', _, _) = withCombinedEnvironment mfs . fmap UA.mkUnitAnnotation $ pf
  let pvm = combinedParamVarMap mfs
    -- Previously we did
  --  FAD.analyseConstExps . FAD.analyseParameterVars pvm
  -- but we do not want to cause constant expression evaluation to 'squeeze out'
  -- constant expressions from the analysis
  let pf'' = FAB.analyseBBlocks $ pf'
  runUnitSolver pf'' $ do
    initializeModFiles
    initInference
    solver

--------------------------------------------------

-- | Seek out any parameters to functions or subroutines that do not
-- already have units, and insert parametric units for them into the
-- map of variables to UnitInfo.
insertParametricUnits :: UnitSolver ()
insertParametricUnits = getProgramFile >>= (mapM_ paramPU . universeBi)
  where
    paramPU pu =
      forM_ (indexedParams pu) $ \ (i, param) ->
        -- Insert a parametric unit if the variable does not already have a unit.
        modifyVarUnitMap $ M.insertWith (curry snd) param (UnitParamPosAbs (fname, param, i))
      where
        fname = (puName pu, puSrcName pu)

-- | Return the list of parameters paired with its positional index.
indexedParams :: F.ProgramUnit UA -> [(Int, VV)]
indexedParams pu
  | F.PUFunction _ _ _ _ _ Nothing (Just r) _ _       <- pu = [(0, toVV r)]
  | F.PUFunction _ _ _ _ _ Nothing _ _ _              <- pu = [(0, (fname, sfname))]
  | F.PUFunction _ _ _ _ _ (Just paList) (Just r) _ _ <- pu = zip [0..] $ map toVV (r : F.aStrip paList)
  | F.PUFunction _ _ _ _ _ (Just paList) _ _ _        <- pu = zip [0..] $ (fname, sfname) : map toVV (F.aStrip paList)
  | F.PUSubroutine _ _ _ _ (Just paList) _ _          <- pu = zip [1..] $ map toVV (F.aStrip paList)
  | otherwise                                               = []
  where
    fname  = puName pu
    sfname = puSrcName pu
    toVV e = (varName e, srcName e)

--------------------------------------------------

-- | Any remaining variables with unknown units are given unit UnitVar
-- with a unique name (in this case, taken from the unique name of the
-- variable as provided by the Renamer), or UnitParamVarAbs if the
-- variables are inside of a function or subroutine.
insertUndeterminedUnits :: UnitSolver ()
insertUndeterminedUnits = do
  pf   <- getProgramFile
  dmap <- lift . lift $ M.union (extractDeclMap pf) . combinedDeclMap <$> analysisModFiles
  forM_ (universeBi pf :: [F.ProgramUnit UA]) $ \ pu ->
    modifyPUBlocksM (transformBiM (insertUndeterminedUnitVar dmap)) pu

-- Specifically handle variables
insertUndeterminedUnitVar :: DeclMap -> F.Expression UA -> UnitSolver (F.Expression UA)
insertUndeterminedUnitVar dmap v@(F.ExpValue _ _ (F.ValVariable _))
  | Just (FA.IDType { FA.idVType = Just sty }) <- FA.idType (F.getAnnotation v)
  , isAcceptableType sty = do
  let vname = varName v
  let sname = srcName v
  let unit  = toUnitVar dmap (vname, sname)
  modifyVarUnitMap $ M.insertWith (curry snd) (varName v, srcName v) unit
  pure v
insertUndeterminedUnitVar _ e = pure e

-- Choose UnitVar or UnitParamVarAbs depending upon how the variable was declared.
toUnitVar :: DeclMap -> VV -> UnitInfo
toUnitVar dmap (vname, sname) = unit
  where
    unit = case fst3 <$> M.lookup vname dmap of
      Just (DCFunction (F.Named fvname, F.Named fsname))   -> UnitParamVarAbs ((fvname, fsname), (vname, sname))
      Just (DCSubroutine (F.Named fvname, F.Named fsname)) -> UnitParamVarAbs ((fvname, fsname), (vname, sname))
      _                                                    -> UnitVar (vname, sname)
    fst3 (a, _, _) = a

-- Insert undetermined units annotations on the following types of variables.
isAcceptableType :: FAS.SemType -> Bool
isAcceptableType = \case
  FAS.TReal    _ -> True
  FAS.TComplex _ -> True
  FAS.TInteger _ -> True
  _              -> False

--------------------------------------------------

-- | Convert explicit polymorphic annotations such as (UnitName "'a")
-- into UnitParamEAPAbs with a 'context-unique-name' given by the
-- ProgramUnitName combined with the supplied unit name.
transformExplicitPolymorphism :: Maybe F.ProgramUnitName -> UnitInfo -> UnitInfo
transformExplicitPolymorphism (Just (F.Named f)) (UnitName a@('\'':_)) = UnitParamEAPAbs (a, f ++ "_" ++ a)
transformExplicitPolymorphism _ u                                      = u

-- | Any units provided by the programmer through comment annotations
-- will be incorporated into the VarUnitMap.
insertGivenUnits :: UnitSolver ()
insertGivenUnits = do
  pf <- getProgramFile
  mapM_ checkPU (universeBi pf)
  where
    -- Look through each Program Unit for the comments
    checkPU :: F.ProgramUnit UA -> UnitSolver ()
    checkPU (F.PUComment a _ _)
      -- Look at unit assignment between function return variable and spec.
      | Just (P.UnitAssignment (Just vars) unitsAST) <- mSpec
      , Just pu                                      <- mPU = insertPUUnitAssigns (toUnitInfo unitsAST) pu vars
      -- Add a new unit alias.
      | Just (P.UnitAlias name unitsAST) <- mSpec = modifyUnitAliasMap (M.insert name (toUnitInfo unitsAST))
      | otherwise                                 = pure ()
      where
        mSpec = UA.unitSpec (FA.prevAnnotation a)
        mPU   = UA.unitPU   (FA.prevAnnotation a)
    -- Other type of ProgramUnit (e.g. one with a body of blocks)
    checkPU pu = mapM_ (checkBlockComment getName) [ b | b@F.BlComment{} <- universeBi (F.programUnitBody pu) ]
      where
        getName = case pu of
          F.PUFunction {}   -> Just $ F.getName pu
          F.PUSubroutine {} -> Just $ F.getName pu
          _                 -> Nothing

    -- Look through each comment that has some kind of unit annotation within it.
    checkBlockComment :: Maybe F.ProgramUnitName -> F.Block UA -> UnitSolver ()
    checkBlockComment pname (F.BlComment a _ _)
      -- Look at unit assignment between variable and spec.
      | Just (P.UnitAssignment (Just vars) unitsAST) <- mSpec
      , Just b                                       <- mBlock = insertBlockUnitAssigns pname (toUnitInfo unitsAST) b vars
      -- Add a new unit alias.
      | Just (P.UnitAlias name unitsAST) <- mSpec  = modifyUnitAliasMap (M.insert name (toUnitInfo unitsAST))
      | otherwise                                  = pure ()
      where
        mSpec  = UA.unitSpec  (FA.prevAnnotation a)
        mBlock = UA.unitBlock (FA.prevAnnotation a)
    checkBlockComment _ _ = error "received non-comment in checkBlockComment"

    -- Figure out the unique names of the referenced variables and
    -- then insert unit info under each of those names.
    insertBlockUnitAssigns :: Maybe F.ProgramUnitName -> UnitInfo -> F.Block UA -> [String] -> UnitSolver ()
    insertBlockUnitAssigns pname info (F.BlStatement _ _ _ (F.StDeclaration _ _ _ _ decls)) varRealNames = do
      -- figure out the 'unique name' of the varRealName that was found in the comment
      -- FIXME: account for module renaming
      -- FIXME: might be more efficient to allow access to variable renaming environ at this program point
      let info' = transform (transformExplicitPolymorphism pname) info
      let m = M.fromList [ ((varName e, srcName e), info')
                         | e@(F.ExpValue _ _ (F.ValVariable _)) <- universeBi decls :: [F.Expression UA]
                         , varRealName <- varRealNames
                         , varRealName == srcName e ]
      modifyVarUnitMap $ M.unionWith const m
      modifyGivenVarSet . S.union . S.fromList . map fst . M.keys $ m
    insertBlockUnitAssigns _ _ _ _ = error "received non-statement/declaration in insertBlockUnitAssigns"

    -- Insert unit annotation for function return variable
    insertPUUnitAssigns :: UnitInfo -> F.ProgramUnit UA -> [String] -> UnitSolver ()
    insertPUUnitAssigns info pu@(F.PUFunction _ _ _ _ _ _ mret _ _) varRealNames
      | (retUniq, retSrc) <- case mret of Just ret -> (FA.varName ret, FA.srcName ret)
                                          Nothing  -> (puName pu, puSrcName pu)
      , retSrc `elem` varRealNames = do
          let pname = Just $ F.getName pu
          let info' = transform (transformExplicitPolymorphism pname) info
          let m = M.fromList [ ((retUniq, retSrc), info') ]
          modifyVarUnitMap $ M.unionWith const m
          modifyGivenVarSet . S.union . S.fromList . map fst . M.keys $ m
    insertPUUnitAssigns _ _ _ = error "received non-function in insertPUUnitAssigns"

--------------------------------------------------

-- | Take the unit information from the VarUnitMap and use it to
-- annotate every variable expression in the AST.
annotateAllVariables :: UnitSolver ()
annotateAllVariables = modifyProgramFileM $ \ pf -> do
  varUnitMap <- getVarUnitMap
  importedVariables <- getImportedVariables
  let varUnitMap' = M.unionWith (curry snd) varUnitMap importedVariables

  let annotateExp e@(F.ExpValue _ _ (F.ValVariable _))
        | Just info <- M.lookup (varName e, srcName e) varUnitMap' = UA.setUnitInfo info e
      -- may need to annotate intrinsics separately
      annotateExp e = e
  pure $ transformBi annotateExp pf

--------------------------------------------------

-- | Give units to literals based upon the rules of the Literals mode.
--
-- LitUnitless: All literals are unitless.
-- LitPoly:     All literals are polymorphic.
-- LitMixed:    The literal "0" or "0.0" is fully parametric polymorphic.
--              All other literals are monomorphic, possibly unitless.
annotateLiterals :: UnitSolver ()
annotateLiterals = modifyProgramFileM (transformBiM annotateLiteralsPU)

annotateLiteralsPU :: F.ProgramUnit UA -> UnitSolver (F.ProgramUnit UA)
annotateLiteralsPU pu = do
  mode <- asks (uoLiterals . unitOpts)
  case mode of
    LitUnitless -> modifyPUBlocksM (transformBiM expUnitless) pu
    LitPoly     -> modifyPUBlocksM (transformBiM (withLiterals genParamLit)) pu
    LitMixed    -> modifyPUBlocksM (transformBiM expMixed) pu
  where
    -- Follow the LitMixed rules.
    expMixed e = case e of
      F.ExpValue _ _ (F.ValInteger i _)
        | read i == 0 -> withLiterals genParamLit e
        | otherwise   -> withLiterals genUnitLiteral e
      F.ExpValue _ _ (F.ValReal i _)
        | readRealLit i == 0.0 -> withLiterals genParamLit e
        | otherwise            -> withLiterals genUnitLiteral e

      F.ExpBinary a s op e1 e2
        | op `elem` [F.Multiplication, F.Division] -> case () of
            -- leave it alone if they're both constants
            _ | Just _ <- constExp (F.getAnnotation e1)
              , Just _ <- constExp (F.getAnnotation e2)        -> pure e
            -- a constant multiplier is unitless
            _ | Just _ <- constExp (F.getAnnotation e1)
              , Just UnitLiteral{} <- UA.getUnitInfo e1         ->
                pure $ F.ExpBinary a s op (UA.setUnitInfo UnitlessLit e1) e2
            -- a constant multiplier is unitless
              | Just _ <- constExp (F.getAnnotation e2)
              , Just UnitLiteral{} <- UA.getUnitInfo e2         ->
                pure $ F.ExpBinary a s op e1 (UA.setUnitInfo UnitlessLit e2)
            _                                                   -> pure e

      _ | Just _ <- constExp (F.getAnnotation e)                -> case UA.getUnitInfo e of
            -- Treat constant expressions as if they were fresh
            -- literals, unless assigned units already.
            Just UnitLiteral{} -> genLit e
            Just UnitVar{}     -> genLit e
            _                  -> pure e
        | otherwise                                             -> pure e

    -- Set all literals to unitless.
    expUnitless e
      | isLiteral e = pure $ UA.setUnitInfo UnitlessLit e
      | otherwise   = pure e

    -- Set all literals to the result of given monadic computation.
    withLiterals m e
      | isLiteral e = flip UA.setUnitInfo e <$> m
      | otherwise   = pure e

    -- isPolyCtxt = case pu of F.PUFunction {} -> True; F.PUSubroutine {} -> True; _ -> False

    genLit e
      | isLiteralZero e = withLiterals genParamLit e
      | otherwise       = withLiterals genUnitLiteral e

-- | Is it a literal, literally?
isLiteral :: F.Expression UA -> Bool
isLiteral (F.ExpValue _ _ F.ValReal{})    = True
isLiteral (F.ExpValue _ _ F.ValInteger{}) = True
-- allow propagated constants to be interpreted as literals
isLiteral e                                 = isJust (constExp (F.getAnnotation e))

-- | Is expression a literal and is it non-zero?
isLiteralNonZero :: F.Expression UA -> Bool
isLiteralNonZero (F.ExpValue _ _ (F.ValInteger i _)) = read        i /= 0
isLiteralNonZero (F.ExpValue _ _ (F.ValReal i _))    = readRealLit i /= 0.0
-- allow propagated constants to be interpreted as literals
isLiteralNonZero e = case constExp (F.getAnnotation e) of
  Just c | Just i <- fromConstInt c -> i /= 0
--  Just (FA.ConstUninterpInt s)  -> read s /= 0
  Just c | Just r <- fromConstReal c -> r /= 0.0
  _                             -> False

isLiteralZero :: F.Expression UA -> Bool
isLiteralZero x = isLiteral x && not (isLiteralNonZero x)

--------------------------------------------------

-- | Filter out redundant constraints.
cullRedundant :: Constraints -> Constraints
cullRedundant = nub . mapMaybe ( \ con -> case con of
  ConEq u1 u2 | u1 /= u2                              -> Just con
  ConConj cs | cs' <- cullRedundant cs, not (null cs) -> Just (ConConj cs')
  _                                                   -> Nothing
  )

-- | Convert all parametric templates into actual uses, via substitution.
applyTemplates :: Constraints -> UnitSolver Constraints
-- postcondition: returned constraints lack all Parametric constructors
applyTemplates cons = do
  dumpConsM "applyTemplates" cons
  -- Get a list of the instances of parametric polymorphism from the constraints.
  let instances = nub [ (name, i) | UnitParamPosUse ((name, _), _, i) <- universeBi cons ]

  -- Also generate a list of 'dummy' instances to ensure that every
  -- 'toplevel' function and subroutine is thoroughly expanded and
  -- analysed, even if it is not used in the current ProgramFile. (It
  -- might be part of a library module, for instance).
  pf <- getProgramFile
  dummies <- forM (topLevelFuncsAndSubs pf) $ \ pu -> do
    ident <- freshId
    pure (puName pu, ident)

  logDebug' pf $ ("instances: " <> describeShow instances)
  logDebug' pf $ ("dummies: " <> describeShow dummies)

  importedVariables <- getImportedVariables

  -- Prepare constraints for all variables imported via StUse.
  let importedCons = [ ConEq (UnitVar vv) units | (vv, units) <- M.toList importedVariables ]

  -- Work through the instances, expanding their templates, and
  -- substituting the callId into the abstract parameters.
  concreteCons <- cullRedundant <$>
                  liftM2 (++) (foldM (substInstance False []) importedCons instances)
                              (foldM (substInstance True []) [] dummies)
  dumpConsM "applyTemplates: concreteCons" concreteCons

  -- Also include aliases in the final set of constraints, where
  -- aliases are implemented by simply asserting that they are equal
  -- to their definition.
  aliasMap <- getUnitAliasMap
  let aliases = [ ConEq (UnitAlias name) def | (name, def) <- M.toList aliasMap ]
  let transAlias (UnitName a) | a `M.member` aliasMap = UnitAlias a
      transAlias u                                    = u

  dumpConsM "aliases" aliases
  pure . transformBi transAlias . cullRedundant $ cons ++ concreteCons ++ aliases

-- | Look up the Parametric templates for a given function or
-- subroutine, and do the substitutions. Process any additional
-- polymorphic calls that are uncovered, unless they are recursive
-- calls that have already been seen in the current call stack.
substInstance :: Bool -> [F.Name] -> Constraints -> (F.Name, Int) -> UnitSolver Constraints
substInstance isDummy callStack output (name, callId) = do
  tmap <- getTemplateMap

  -- Look up the templates associated with the given function or
  -- subroutine name. And then transform the templates by generating
  -- new callIds for any constraints created by function or subroutine
  -- calls contained within the templates.
  --
  -- The reason for this is because functions called by functions can
  -- be used in a parametric polymorphic way.

  -- npc <- nameParamConstraints name -- In case it is an imported function, use this.
  let npc = [] -- disabled for now
  template <- transformBiM callIdRemap $ npc `fromMaybe` M.lookup name tmap
  dumpConsM ("substInstance " ++ show isDummy ++ " " ++ show callStack ++ " " ++ show (name, callId) ++ " template lookup") template

  -- Reset the usCallIdRemap field so that it is ready for the next
  -- set of templates.
  modifyCallIdRemap (const IM.empty)

  -- If any new instances are discovered, also process them, unless recursive.
  let instances = nub [ (name', i) | UnitParamPosUse ((name', _), _, i) <- universeBi template ]
  template' <- if name `elem` callStack then
                 -- Detected recursion: we do not support polymorphic-unit recursion,
                 -- ergo all subsequent recursive calls are assumed to have the same
                 -- unit-assignments as the first call.
                 pure []
               else
                 foldM (substInstance False (name:callStack)) [] instances

  dumpConsM ("instantiating " ++ show (name, callId) ++ ": (output ++ template) is") (output ++ template)
  dumpConsM ("instantiating " ++ show (name, callId) ++ ": (template') is") template'

  -- Convert abstract parametric units into concrete ones.

  let output' = -- Do not instantiate explicitly annotated polymorphic
                -- variables from current context when looking at dummy (name, callId)
                (if isDummy then output ++ template
                            else instantiate callId (output ++ template)) ++

                -- Only instantiate explicitly annotated polymorphic
                -- variables from nested function/subroutine calls.
                instantiate callId template'

  dumpConsM ("final output for " ++ show (name, callId)) output'

  pure output'

-- -- | Generate constraints from a NameParamMap entry.
-- nameParamConstraints :: F.Name -> UnitSolver Constraints
-- nameParamConstraints fname = do
--   let filterForName (NPKParam (n, _) _) _ = n == fname
--       filterForName _ _                   = False
--   nlst <- (M.toList . M.filterWithKey filterForName) <$> getNameParamMap
--   pure [ ConEq (UnitParamPosAbs (n, pos)) (foldUnits units) | (NPKParam n pos, units) <- nlst ]

-- | If given a usage of a parametric unit, rewrite the callId field
-- to follow an existing mapping in the usCallIdRemap state field, or
-- generate a new callId and add it to the usCallIdRemap state field.
callIdRemap :: UnitInfo -> UnitSolver UnitInfo
callIdRemap info = modifyCallIdRemapM $ \ idMap -> case info of
    UnitParamPosUse (n, p, i)
      | Just i' <- IM.lookup i idMap -> pure (UnitParamPosUse (n, p, i'), idMap)
      | otherwise                    -> freshId >>= \ i' ->
                                          pure (UnitParamPosUse (n, p, i'), IM.insert i i' idMap)
    UnitParamVarUse (n, v, i)
      | Just i' <- IM.lookup i idMap -> pure (UnitParamVarUse (n, v, i'), idMap)
      | otherwise                    -> freshId >>= \ i' ->
                                          pure (UnitParamVarUse (n, v, i'), IM.insert i i' idMap)
    UnitParamLitUse (l, i)
      | Just i' <- IM.lookup i idMap -> pure (UnitParamLitUse (l, i'), idMap)
      | otherwise                    -> freshId >>= \ i' ->
                                          pure (UnitParamLitUse (l, i'), IM.insert i i' idMap)
    UnitParamEAPUse (v, i)
      | Just i' <- IM.lookup i idMap -> pure (UnitParamEAPUse (v, i'), idMap)
      | otherwise                    -> freshId >>= \ i' ->
                                          pure (UnitParamEAPUse (v, i'), IM.insert i i' idMap)

    _                                -> pure (info, idMap)


-- | Convert a parametric template into a particular use.
instantiate :: Data a => Int -> a -> a
instantiate callId = transformBi $ \ info -> case info of
  UnitParamPosAbs (puname, _argname, position) -> UnitParamPosUse (puname, position, callId)
  UnitParamLitAbs litId            -> UnitParamLitUse (litId, callId)
  UnitParamVarAbs (fname, vname)   -> UnitParamVarUse (fname, vname, callId)
  UnitParamEAPAbs vname            -> UnitParamEAPUse (vname, callId)
  _                                -> info

-- | Return a list of ProgramUnits that might be considered 'toplevel'
-- in the ProgramFile, e.g., possible exports. These must be analysed
-- independently of whether they are actually used in the same file,
-- because other files might use them.
topLevelFuncsAndSubs :: F.ProgramFile a -> [F.ProgramUnit a]
topLevelFuncsAndSubs (F.ProgramFile _ pus) = topLevel =<< pus
  where
    topLevel (F.PUModule _ _ _ _ (Just contains)) = topLevel =<< contains
    topLevel (F.PUMain _ _ _ _ (Just contains))   = topLevel =<< contains
    topLevel f@F.PUFunction{}                  = pure f
    topLevel s@F.PUSubroutine{}                = pure s
    topLevel _                                    = []

--------------------------------------------------

-- | Gather all constraints from the main blocks of the AST, as well as from the varUnitMap
extractConstraints :: UnitSolver Constraints
extractConstraints = do
  pf         <- getProgramFile
  dmap       <- lift . lift $ M.union (extractDeclMap pf) . combinedDeclMap <$> analysisModFiles
  varUnitMap <- getVarUnitMap
  pure $ [ con | b <- mainBlocks pf, con@ConEq{} <- universeBi b ] ++
           [ ConEq (toUnitVar dmap v) u | (v, u) <- M.toList varUnitMap ]

-- | A list of blocks considered to be part of the 'main' program.
mainBlocks :: F.ProgramFile UA -> [F.Block UA]
mainBlocks = concatMap getBlocks . universeBi
  where
    getBlocks (F.PUMain _ _ _ bs _)   = bs
    getBlocks (F.PUModule _ _ _ bs _) = bs
    getBlocks _                       = []

--------------------------------------------------

-- | Propagate* functions: decorate the AST with constraints, given
-- that variables have all been annotated.
propagateUnits :: UnitSolver ()
-- precondition: all variables have already been annotated
propagateUnits = modifyProgramFileM $ transformBiM propagateInterface <=<
                                      transformBiM propagatePU        <=<
                                      transformBiM propagateDoSpec    <=<
                                      transformBiM propagateStatement <=<
                                      transformBiM propagateExp

propagateExp :: F.Expression UA -> UnitSolver (F.Expression UA)
propagateExp e = case e of
  F.ExpValue{}                           -> pure e -- all values should already be annotated
  F.ExpBinary _ _ F.Multiplication e1 e2 -> setF2 UnitMul (UA.getUnitInfo e1) (UA.getUnitInfo e2)
  F.ExpBinary _ _ F.Division e1 e2       -> setF2 UnitMul (UA.getUnitInfo e1) (flip UnitPow (-1) <$> UA.getUnitInfo e2)
  F.ExpBinary _ _ F.Exponentiation e1 e2 -> setF2 UnitPow (UA.getUnitInfo e1) (constantExpression e2)
  F.ExpBinary _ _ o e1 e2 | isOp AddOp o -> setF2C ConEq  (UA.getUnitInfo e1) (UA.getUnitInfo e2)
                          | isOp RelOp o -> setF2C ConEq  (UA.getUnitInfo e1) (UA.getUnitInfo e2)
  F.ExpFunctionCall {}                   -> propagateFunctionCall e
  F.ExpSubscript _ _ e1 _                -> pure $ UA.maybeSetUnitInfo (UA.getUnitInfo e1) e
  F.ExpUnary _ _ _ e1                    -> pure $ UA.maybeSetUnitInfo (UA.getUnitInfo e1) e
  F.ExpInitialisation{}                  -> pure e
  _                                      -> do
    logDebug' e $ "progagateExp: unhandled " <> describeShow e
    pure e
  where
    -- Shorter names for convenience functions.
    setF2 f u1 u2  = pure $ UA.maybeSetUnitInfoF2 f u1 u2 e
    -- Remember, not only set a constraint, but also give a unit!
    setF2C f u1 u2 = pure . UA.maybeSetUnitInfo u1 $ UA.maybeSetUnitConstraintF2 f u1 u2 e

propagateFunctionCall :: F.Expression UA -> UnitSolver (F.Expression UA)
propagateFunctionCall (F.ExpFunctionCall a s f (F.AList a' s' args)) = do
  (info, args') <- callHelper f args
  let cons = intrinsicHelper info f args'
  pure . UA.setConstraint (ConConj cons) . UA.setUnitInfo info $ F.ExpFunctionCall a s f (F.AList a' s' args')
propagateFunctionCall _ = error "received non-function-call in propagateFunctionCall"

propagateDoSpec :: F.DoSpecification UA -> UnitSolver (F.DoSpecification UA)
propagateDoSpec ast@(F.DoSpecification _ _ (F.StExpressionAssign _ _ e1 _) e2 m_e3) = do
  -- express constraints between the iteration variable, the bounding
  -- expressions and the step expression, or treat the step expression
  -- as a literal 1 if not specified.
  pure . maybe ast (flip UA.setConstraint ast) $ ConConj <$> mconcat [
        -- units(e1) ~ units(e2)
        (:[]) <$> liftM2 ConEq (UA.getUnitInfo e1) (UA.getUnitInfo e2)

        -- units(e1) ~ units(e3) or if e3 not specified then units(e1) ~ 1 in a polymorphic context
        , do u1 <- UA.getUnitInfo e1
             u3 <- (UA.getUnitInfo =<< m_e3) `mplus` if isMonomorphic u1 then mzero else pure UnitlessVar
             pure [ConEq u1 u3]

        -- units(e2) ~ units(e3) or if e3 not specified then units(e2) ~ 1 in a polymorphic context
        , do u2 <- UA.getUnitInfo e1
             u3 <- (UA.getUnitInfo =<< m_e3) `mplus` if isMonomorphic u2 then mzero else pure UnitlessVar
             pure [ConEq u2 u3]
        ]
propagateDoSpec _ = error "propagateDoSpec: called on invalid DoSpec"

propagateStatement :: F.Statement UA -> UnitSolver (F.Statement UA)
propagateStatement stmt = case stmt of
  F.StExpressionAssign _ _ e1 e2               -> literalAssignmentSpecialCase e1 e2 stmt
  F.StCall a s sub (F.AList a' s' args) -> do
    (info, args') <- callHelper sub args
    let cons = intrinsicHelper info sub args'
    pure . UA.setConstraint (ConConj cons) $ F.StCall a s sub (F.AList a' s' args')
  F.StDeclaration {}                           -> transformBiM propagateDeclarator stmt
  _                                            -> pure stmt

propagateDeclarator :: F.Declarator UA -> UnitSolver (F.Declarator UA)
propagateDeclarator decl = case decl of
  F.Declarator _ _ e1 _ _ (Just e2) -> literalAssignmentSpecialCase e1 e2 decl
  F.Declarator _ _ e1 _ _ Nothing   -> do
    -- Handle uninitialized variables
    opts <- ask
    if uninitializeds . unitOpts $ opts
      then do
        -- Find the enclosing program unit
        punames <- lookupEnclosingPUname (FA.varName e1)
        pure $
          -- Set up a param var abstract constraint
          UA.maybeSetUnitConstraintF2
             ConEq
               (UA.getUnitInfo e1)
               (Just $ UnitParamVarAbs (punames, (FA.varName e1, FA.srcName e1)))
               decl
      else pure decl

-- Helper to lookup the name of the enclosing program unit
lookupEnclosingPUname :: F.Name -> UnitSolver (F.Name, F.Name)
lookupEnclosingPUname internalName = do
    st <- get
    return . aux $ (F.programFileProgramUnits . usProgramFile $ st)
  where
    aux :: [ F.ProgramUnit UA ] -> (F.Name, F.Name)
    -- Note this is partial but the declaration should exist
    aux (pu : pus) =
      case hits of
        [] -> aux pus
        (hit:_) -> hit
      where
        -- Find matching decls - zero or more
        hits =
          [ (puName pu, puSrcName pu) |
            -- Find all decls
            decl'@F.Declarator{} <- universeBi pu :: [F.Declarator UA]
            -- Filter by those matching the internal name of the declaration
          , internalName == (FA.varName $ F.declaratorVariable decl') ]


-- Allow literal assignment to overload the non-polymorphic
-- unit-assignment of the non-zero literal.
literalAssignmentSpecialCase :: (F.Annotated f)
                             => F.Expression UA -> F.Expression UA
                             -> f UA -> UnitSolver (f UA)
literalAssignmentSpecialCase e1 e2 ast
  | isLiteralZero e2 = pure ast
  | isLiteral e2
  , Just u1            <- UA.getUnitInfo e1
  , Just UnitLiteral{} <- UA.getUnitInfo e2
  , isMonomorphic u1 = pure ast
  -- otherwise express the constraint between LHS and RHS of assignment.
  | otherwise = pure $ UA.maybeSetUnitConstraintF2 ConEq (UA.getUnitInfo e1) (UA.getUnitInfo e2) ast

-- Generic Interface template mapping will be same as first module procedure.
propagateInterface :: F.Block UA -> UnitSolver (F.Block UA)
propagateInterface b@(F.BlInterface _ _ (Just e) _ _ bs) = do
  let iname = varName e
  case [ varName e1 | F.StModuleProcedure _ _ (F.AList _ _ (e1:_)) <- universeBi bs :: [F.Statement UA] ] of
    mpname:_ -> do
      -- translate any instance of mpname into iname within the template
      let trans = transformBi (\ x -> if x == mpname then iname else x)
      -- copy (translated) template from first module procedure to interface
      modifyTemplateMap $ \ m -> fromMaybe m ((\ t -> M.insert iname (trans t) m) <$> M.lookup mpname m)
    _        ->
      pure ()
  pure b

propagateInterface b = pure b

propagatePU :: F.ProgramUnit UA -> UnitSolver (F.ProgramUnit UA)
propagatePU pu = do
  let name     = puName pu
  let sname    = puSrcName pu
  let nn       = (name, sname)
  let bodyCons = [ con | con@ConEq{} <- universeBi pu ] -- Constraints within the PU.

  varMap <- getVarUnitMap

  -- If any of the function/subroutine parameters was given an
  -- explicit unit annotation, then create a constraint between that
  -- explicit unit and the UnitParamPosAbs corresponding to the
  -- parameter. This way all other uses of the parameter get linked to
  -- the explicit unit annotation as well.
  givenCons <- forM (indexedParams pu) $ \ (i, param) ->
    case M.lookup param varMap of
      Just (UnitParamPosAbs (_, vname, _)) -> pure . ConEq (UnitParamVarAbs (nn, param)) $ UnitParamPosAbs (nn, param, i)
      Just u                    -> pure . ConEq u $ UnitParamPosAbs (nn, param, i)
      _                         -> pure . ConEq (UnitParamVarAbs (nn, param)) $ UnitParamPosAbs (nn, param, i)

  let cons = givenCons ++ bodyCons
  case pu of F.PUFunction {}   -> modifyTemplateMap (M.insert name cons)
             F.PUSubroutine {} -> modifyTemplateMap (M.insert name cons)
             _                 -> pure ()

  -- Set the unitInfo field of a function program unit to be the same
  -- as the unitInfo of its result.
  let pu' = case (pu, indexedParams pu) of
              (F.PUFunction {}, (0, res):_) -> UA.setUnitInfo (UnitParamPosAbs (nn, res, 0) `fromMaybe` M.lookup res varMap) pu
              _                             -> pu

  pure (UA.setConstraint (ConConj cons) pu')

--------------------------------------------------

-- | Coalesce various function and subroutine call common code.
callHelper :: F.Expression UA -> [F.Argument UA] -> UnitSolver (UnitInfo, [F.Argument UA])
callHelper nexp args = do
  let name = (varName nexp, srcName nexp)
  let ctyp = FA.idCType =<< FA.idType (F.getAnnotation nexp)
  callId <- case ctyp of
    Just FA.CTExternal -> pure 0  -- if external with no further info then no polymorphism
    _                  -> freshId -- every call-site gets its own unique identifier
  let eachArg i arg@(F.Argument _ _ _ e)
        -- add site-specific parametric constraints to each argument
        | Just u <- UA.getUnitInfo e = UA.setConstraint (ConEq u (UnitParamPosUse (name, i, callId))) arg
        | otherwise               = arg
  let args' = zipWith eachArg [1..] args
  -- build a site-specific parametric unit for use on a return variable, if any
  let info = UnitParamPosUse (name, 0, callId)
  pure (info, args')

-- FIXME: use this function to create a list of constraints on intrinsic call-sites...
intrinsicHelper :: Foldable t => UnitInfo -> F.Expression (FA.Analysis a) -> t b -> [Constraint]
intrinsicHelper (UnitParamPosUse (_, _, callId)) f@(F.ExpValue _ _ (F.ValIntrinsic _)) args
  | Just (retU, argUs) <- intrinsicLookup sname = zipWith eachArg [0..numArgs] (retU:argUs)
  where
    numArgs     = length args
    sname       = srcName f
    vname       = varName f
    eachArg i u = ConEq (UnitParamPosUse ((vname, sname), i, callId)) (instantiate callId u)
intrinsicHelper _ _ _ = []

-- | Get info about intrinsics by source name 'sname', taking into
-- account the special case of those with arbitrary number of
-- arguments.
intrinsicLookup :: F.Name -> Maybe (UnitInfo, [UnitInfo])
intrinsicLookup sname = do
  (retU, argUs) <- M.lookup sname intrinsicUnits
  return (retU, if sname `S.member` specialCaseArbitraryArgs then cycle argUs else argUs)

-- | Generate a unique identifier for a literal encountered in the code.
genUnitLiteral :: UnitSolver UnitInfo
genUnitLiteral = UnitLiteral <$> freshId

-- | Generate a unique identifier for a polymorphic literal encountered in the code.
genParamLit :: UnitSolver UnitInfo
genParamLit = UnitParamLitAbs <$> freshId

-- Operate only on the blocks of a program unit, not the contained sub-programunits.
modifyPUBlocksM :: Monad m => ([F.Block a] -> m [F.Block a]) -> F.ProgramUnit a -> m (F.ProgramUnit a)
modifyPUBlocksM f pu = case pu of
  F.PUMain a s n b pus                    -> flip fmap (f b) $ \ b' -> F.PUMain a s n b' pus
  F.PUModule a s n b pus                  -> flip fmap (f b) $ \ b' -> F.PUModule a s n b' pus
  F.PUSubroutine a s r n p b subs         -> flip fmap (f b) $ \ b' -> F.PUSubroutine a s r n p b' subs
  F.PUFunction   a s r rec n p res b subs -> flip fmap (f b) $ \ b' -> F.PUFunction a s r rec n p res b' subs
  F.PUBlockData  a s n b                  -> flip fmap (f b) $ \ b' -> F.PUBlockData  a s n b'
  F.PUComment {}                          -> pure pu -- no blocks

-- Fortran semantics for interpretation of constant expressions
-- involving numeric literals.
data FNum = FReal Double | FInt Integer

fnumToDouble :: FNum -> Double
fnumToDouble (FReal x) = x
fnumToDouble (FInt x)  = fromIntegral x

fAdd, fSub, fMul, fDiv, fPow :: FNum -> FNum -> FNum
fAdd (FReal x) fy      = FReal $ x + fnumToDouble fy
fAdd fx (FReal y)      = FReal $ fnumToDouble fx + y
fAdd (FInt x) (FInt y) = FInt  $ x + y
fSub (FReal x) fy      = FReal $ x - fnumToDouble fy
fSub fx (FReal y)      = FReal $ fnumToDouble fx - y
fSub (FInt x) (FInt y) = FInt  $ x - y
fMul (FReal x) fy      = FReal $ x * fnumToDouble fy
fMul fx (FReal y)      = FReal $ fnumToDouble fx * y
fMul (FInt x) (FInt y) = FInt  $ x * y
fDiv (FReal x) fy      = FReal $ x / fnumToDouble fy
fDiv fx (FReal y)      = FReal $ fnumToDouble fx / y
fDiv (FInt x) (FInt y) = FInt  $ x `quot` y  -- Haskell quot truncates towards zero, like Fortran
fPow (FReal x) fy      = FReal $ x ** fnumToDouble fy
fPow fx (FReal y)      = FReal $ fnumToDouble fx ** y
fPow (FInt x) (FInt y)
  | y >= 0             = FInt  $ x ^ y
  | otherwise          = FReal $ fromIntegral x ^^ y

fDivMaybe :: Maybe FNum -> Maybe FNum -> Maybe FNum
fDivMaybe mx my
  | Just y <- my,
    fnumToDouble y == 0.0 = Nothing
  | otherwise             = liftM2 fDiv mx my

-- | Statically computes if the expression is a constant value.
constantExpression :: F.Expression a -> Maybe Double
constantExpression expr = fnumToDouble <$> ce expr
  where
    ce e = case e of
      (F.ExpValue _ _ (F.ValInteger i _))      -> Just $ FInt $ read i
      (F.ExpValue _ _ (F.ValReal r _))         -> Just $ FReal $ readRealLit r
      (F.ExpBinary _ _ F.Addition e1 e2)       -> liftM2 fAdd (ce e1) (ce e2)
      (F.ExpBinary _ _ F.Subtraction e1 e2)    -> liftM2 fSub (ce e1) (ce e2)
      (F.ExpBinary _ _ F.Multiplication e1 e2) -> liftM2 fMul (ce e1) (ce e2)
      (F.ExpBinary _ _ F.Division e1 e2)       -> fDivMaybe (ce e1) (ce e2)
      (F.ExpBinary _ _ F.Exponentiation e1 e2) -> liftM2 fPow (ce e1) (ce e2)
      -- FIXME: expand...
      _                                        -> Nothing

-- | Asks the question: is the operator within the given category?
isOp :: BinOpKind -> F.BinaryOp -> Bool
isOp cat = (== cat) . binOpKind

data BinOpKind = AddOp | MulOp | DivOp | PowerOp | LogicOp | RelOp deriving Eq
binOpKind :: F.BinaryOp -> BinOpKind
binOpKind F.Addition         = AddOp
binOpKind F.Subtraction      = AddOp
binOpKind F.Multiplication   = MulOp
binOpKind F.Division         = DivOp
binOpKind F.Exponentiation   = PowerOp
binOpKind F.Concatenation    = AddOp
binOpKind F.GT               = RelOp
binOpKind F.GTE              = RelOp
binOpKind F.LT               = RelOp
binOpKind F.LTE              = RelOp
binOpKind F.EQ               = RelOp
binOpKind F.NE               = RelOp
binOpKind F.Or               = LogicOp
binOpKind F.And              = LogicOp
binOpKind F.XOr              = LogicOp
binOpKind F.Equivalent       = RelOp
binOpKind F.NotEquivalent    = RelOp
binOpKind (F.BinCustom _)    = RelOp

-- | Get information about imported variables coming from mod files.
getImportedVariables :: UnitSolver (M.Map VV UnitInfo)
getImportedVariables = do
  pf <- getProgramFile
  nmap <- getNameParamMap
  -- Translate a Use AST node into a pair mapping unique name to 'local' source name in this program file.
  let useToPair (F.UseID _ _ e) = (varName e, srcName e)
      useToPair (F.UseRename _ _ e1 _) = (varName e1, srcName e1) -- (unique name, 'local' source name)
  -- A map of modules -> (maps of variables -> their unit info).
  let modnmaps = [ M.fromList (mapMaybe f (M.toList npkmap))
                 -- find all StUse statements and identify variables that need to be imported from nmap
                 | F.StUse _ _ e _ only alist <- universeBi pf :: [ F.Statement UA ]
                 , let mod = srcName e
                 , let uses = map useToPair (fromMaybe [] (F.aStrip <$> alist))
                 , Just npkmap <- [M.lookup (F.Named mod) nmap]
                 , let f (npk, ui) = case npk of
                         (NPKVariable (var, src))
                           -- import all variables from module -- apply any renames from uses
                           | only == F.Permissive -> Just (NPKVariable (var, src `fromMaybe` lookup var uses), ui)
                           -- only import variable mentioned in uses
                           | Just src' <- lookup var uses -> Just (NPKVariable (var, src'), ui)
                         _ -> Nothing
                 ]
  pure $ M.fromList [ (vv, foldUnits units) | (NPKVariable vv, units) <- M.toList (M.unions modnmaps) ]

--------------------------------------------------

logDebugNoOrigin :: Text -> UnitSolver ()
logDebugNoOrigin msg = do
  pf <- gets usProgramFile
  logDebug' pf msg

dumpConsM :: String -> Constraints -> UnitSolver ()
dumpConsM str = logDebugNoOrigin . describe . unlines . ([replicate 50 '-', str ++ ":"]++) . (++[replicate 50 '^']) . map f
  where
    f (ConEq u1 u2)  = show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2)
    f (ConConj cons) = intercalate " && " (map f cons)

debugLogging :: UnitSolver ()
debugLogging = do
    (logDebugNoOrigin . describe . unlines . map (\ (ConEq u1 u2) -> "  ***AbsConstraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2))) =<< extractConstraints
    pf   <- getProgramFile
    cons <- getConstraints
    vum  <- getVarUnitMap
    logDebugNoOrigin . describe . unlines $ [ "  " ++ show info ++ " :: " ++ n | ((n, _), info) <- M.toList vum ]
    logDebugNoOrigin ""
    uam <- getUnitAliasMap
    logDebugNoOrigin . describe . unlines $ [ "  " ++ n ++ " = " ++ show info | (n, info) <- M.toList uam ]
    logDebugNoOrigin . describe . unlines $ map (\ (ConEq u1 u2) -> "  ***Constraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2)) cons
    logDebugNoOrigin $ describeShow cons <> "\n"
    forM_ (universeBi pf) $ \ pu -> case pu of
      F.PUFunction {}
        | Just (ConConj con) <- UA.getConstraint pu ->
          logDebugNoOrigin . describe . unlines $ (puName pu ++ ":"):map (\ (ConEq u1 u2) -> "    constraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2)) con
      F.PUSubroutine {}
        | Just (ConConj con) <- UA.getConstraint pu ->
          logDebugNoOrigin . describe . unlines $ (puName pu ++ ":"):map (\ (ConEq u1 u2) -> "    constraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2)) con
      _ -> pure ()
    let (lhsM, rhsM, _, lhsColA, rhsColA) = constraintsToMatrices cons
    logDebugNoOrigin "--------------------------------------------------\nLHS Cols:"
    logDebugNoOrigin $ describeShow lhsColA
    logDebugNoOrigin "--------------------------------------------------\nRHS Cols:"
    logDebugNoOrigin $ describeShow rhsColA
    logDebugNoOrigin "--------------------------------------------------\nLHS M:"
    logDebugNoOrigin $ describeShow lhsM
    logDebugNoOrigin "--------------------------------------------------\nRHS M:"
    logDebugNoOrigin $ describeShow rhsM
    logDebugNoOrigin "--------------------------------------------------\nAUG M:"
    let augM = if H.rows rhsM == 0 || H.cols rhsM == 0 then lhsM else H.fromBlocks [[lhsM, rhsM]]
    logDebugNoOrigin $ describeShow augM
    logDebugNoOrigin "--------------------------------------------------\nSolved (hnf) M:"
    let hnfM = Flint.hnf augM
    logDebugNoOrigin $ describeShow hnfM
    logDebugNoOrigin "--------------------------------------------------\nSolved (normHNF) M:"
    let (solvedM, newColIndices) = Flint.normHNF augM
    logDebugNoOrigin . describeShow $ solvedM
    logDebugNoOrigin $ "newColIndices = " <> describeShow newColIndices
    logDebugNoOrigin "--------------------------------------------------\nLHS Cols with newColIndices:"
    let lhsCols = A.elems lhsColA ++ map (lhsColA A.!) newColIndices
    logDebugNoOrigin $ describe . unlines . map show $ zip [(0::Int)..] lhsCols
    -- logDebugNoOrigin "--------------------------------------------------\nSolved (SVD) M:"
    -- logDebugNoOrigin $ show (H.linearSolveSVD lhsM rhsM)
    -- logDebugNoOrigin "--------------------------------------------------\nSingular Values:"
    -- logDebugNoOrigin $ show (H.singularValues lhsM)
    logDebugNoOrigin "--------------------------------------------------"
    logDebugNoOrigin $ "Rank LHS: " <> describeShow (H.rank lhsM)
    logDebugNoOrigin "--------------------------------------------------"
    let augA = if H.rows rhsM == 0 || H.cols rhsM == 0 then lhsM else H.fromBlocks [[lhsM, rhsM]]
    logDebugNoOrigin $ "Rank Augmented: " <> describeShow (H.rank augA)
    logDebugNoOrigin "--------------------------------------------------\nGenUnitAssignments:"
    let unitAssignments = genUnitAssignments cons
    logDebugNoOrigin . describe . unlines $ map (\ (u1s, u2) -> "  ***UnitAssignment: " ++ show u1s ++ " === " ++ show (flattenUnits u2) ++ "\n") unitAssignments
    logDebugNoOrigin "--------------------------------------------------"
    let unitAssignmentsSBV = BackendSBV.genUnitAssignments cons
    logDebugNoOrigin . describe . unlines $ map (\ (u1s, u2) -> "  ***UnitAssignmentSBV: " ++ show u1s ++ " === " ++ show (flattenUnits u2)) unitAssignmentsSBV
    logDebugNoOrigin "--------------------------------------------------\nProvenance:"
    let (augM', p) = provenance augM
    logDebugNoOrigin . describeShow $ augM'
    logDebugNoOrigin . describeShow $ p

--------------------------------------------------

-- convenience
puName :: F.ProgramUnit UA -> F.Name
puName pu
  | F.Named n <- FA.puName pu = n
  | otherwise                 = "_nameless"

puSrcName :: F.ProgramUnit UA -> F.Name
puSrcName pu
  | F.Named n <- FA.puSrcName pu = n
  | otherwise                    = "_nameless"

--------------------------------------------------

-- | Intrinics that take arbitrary number of arguments. Entry in table
-- 'intrinsicUnits' will contain a single item in the argument list,
-- corresponding to the template used for all arguments.
specialCaseArbitraryArgs :: S.Set F.Name
specialCaseArbitraryArgs = S.fromList [ "max", "max0", "amax1", "dmax1", "amax0", "max1"
                                      , "min", "min0", "amin1", "dmin1", "amin0", "min1" ]

-- | Intrinsics table: name => (return-unit, parameter-units). See also 'specialCaseArbitraryArgs'.
intrinsicUnits :: M.Map F.Name (UnitInfo, [UnitInfo])
intrinsicUnits =
  M.fromList
    [ ("transfer", (UnitParamEAPAbs ("'b", "'b"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'b", "'b")]))
    , ("abs", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("iabs", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("dabs", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("cabs", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("aimag", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("aint", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("dint", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("anint", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("dnint", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("cmplx", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("conjg", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("dble", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("dim", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'a", "'a")]))
    , ("idim", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'a", "'a")]))
    , ("ddim", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'a", "'a")]))
    , ("dprod", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("ceiling", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("floor", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("int", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("ifix", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("idint", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("maxval", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("minval", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("max", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))   -- special case: arbitrary # of parameters
    , ("min", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))   -- special case: arbitrary # of parameters
    , ("min0", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))  -- special case: arbitrary # of parameters
    , ("amin1", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")])) -- special case: arbitrary # of parameters
    , ("dmin1", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")])) -- special case: arbitrary # of parameters
    , ("amin0", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")])) -- special case: arbitrary # of parameters
    , ("min1", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))  -- special case: arbitrary # of parameters
    , ("mod", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'b", "'b")]))
    , ("modulo", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'b", "'b")]))
    , ("amod", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'b", "'b")]))
    , ("dmod", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'b", "'b")]))
    , ("nint", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("real", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("float", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("sngl", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
    , ("sign", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'b", "'b")]))
    , ("isign", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'b", "'b")]))
    , ("dsign", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'b", "'b")]))
    , ("present", (UnitParamEAPAbs ("'a", "'a"), [UnitlessVar]))
    , ("sqrt", (UnitParamEAPAbs ("'a", "'a"), [UnitPow (UnitParamEAPAbs ("'a", "'a")) 2]))
    , ("dsqrt", (UnitParamEAPAbs ("'a", "'a"), [UnitPow (UnitParamEAPAbs ("'a", "'a")) 2]))
    , ("csqrt", (UnitParamEAPAbs ("'a", "'a"), [UnitPow (UnitParamEAPAbs ("'a", "'a")) 2]))
    , ("exp", (UnitlessVar, [UnitlessVar]))
    , ("dexp", (UnitlessVar, [UnitlessVar]))
    , ("cexp", (UnitlessVar, [UnitlessVar]))
    , ("alog", (UnitlessVar, [UnitlessVar]))
    , ("dlog", (UnitlessVar, [UnitlessVar]))
    , ("clog", (UnitlessVar, [UnitlessVar]))
    , ("alog10", (UnitlessVar, [UnitlessVar]))
    , ("dlog10", (UnitlessVar, [UnitlessVar]))
    , ("sin", (UnitlessVar, [UnitlessVar]))
    , ("dsin", (UnitlessVar, [UnitlessVar]))
    , ("csin", (UnitlessVar, [UnitlessVar]))
    , ("cos", (UnitlessVar, [UnitlessVar]))
    , ("dcos", (UnitlessVar, [UnitlessVar]))
    , ("ccos", (UnitlessVar, [UnitlessVar]))
    , ("tan", (UnitlessVar, [UnitlessVar]))
    , ("dtan", (UnitlessVar, [UnitlessVar]))
    , ("asin", (UnitlessVar, [UnitlessVar]))
    , ("dasin", (UnitlessVar, [UnitlessVar]))
    , ("acos", (UnitlessVar, [UnitlessVar]))
    , ("dacos", (UnitlessVar, [UnitlessVar]))
    , ("atan", (UnitlessVar, [UnitlessVar]))
    , ("datan", (UnitlessVar, [UnitlessVar]))
    , ("atan2", (UnitlessVar, [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'a", "'a")]))
    , ("datan2", (UnitlessVar, [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'a", "'a")]))
    , ("sinh", (UnitlessVar, [UnitlessVar]))
    , ("dsinh", (UnitlessVar, [UnitlessVar]))
    , ("cosh", (UnitlessVar, [UnitlessVar]))
    , ("dcosh", (UnitlessVar, [UnitlessVar]))
    , ("tanh", (UnitlessVar, [UnitlessVar]))
    , ("dtanh", (UnitlessVar, [UnitlessVar]))
    , ("iand", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a"), UnitParamEAPAbs ("'a", "'a")]))
    ]

-- Others: reshape, merge need special handling

-- | Compile a program to a 'ModFile' containing units information.
compileUnits :: UnitOpts -> ModFiles -> F.ProgramFile Annotation -> IO ModFile
compileUnits uo mfs pf = do
  let (pf', _, _) = withCombinedEnvironment mfs . fmap UA.mkUnitAnnotation $ pf

  let analysis = runReaderT (runInference runCompileUnits) $
        UnitEnv
        { unitOpts = uo
        , unitProgramFile = pf
        }

  report <- runAnalysisT (F.pfGetFilename pf) (logOutputNone True) LogError mfs analysis

  case report ^? arResult . _ARSuccess . _1 of
    Just cu -> return (genUnitsModFile pf' cu)
    Nothing -> fail "compileUnits: units analysis failed"
