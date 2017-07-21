{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-
  Units of measure extension to Fortran: frontend
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Camfort.Specification.Units.InferenceFrontend
  ( initInference
  , puName
  , puSrcName
  , runCompileUnits
  , runCriticalVariables
  , runInconsistentConstraints
  , runInferVariables
  ) where

import Data.Data (Data)
import Data.List (nub, intercalate, partition)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.Maybe (isJust, fromMaybe)
import Data.Generics.Uniplate.Operations
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.RWS.Strict

import qualified Language.Fortran.AST as F
import Language.Fortran.Parser.Utils (readReal, readInteger)
import Language.Fortran.Util.Position (getSpan)
import Language.Fortran.Util.ModFile
import qualified Language.Fortran.Analysis as FA
import Language.Fortran.Analysis (varName, srcName)

import Camfort.Analysis.CommentAnnotator (annotateComments)
import Camfort.Analysis.Annotations
import qualified Camfort.Specification.Units.Annotation as UA
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Monad
import Camfort.Specification.Units.InferenceBackend
import Camfort.Specification.Units.Parser (unitParser)
import qualified Camfort.Specification.Units.Parser.Types as P

import qualified Debug.Trace as D
import qualified Numeric.LinearAlgebra as H -- for debugging

--------------------------------------------------

-- | Prepare to run an inference function.
initInference :: UnitSolver ()
initInference = do
  pf <- gets usProgramFile

  -- Parse unit annotations found in comments and link to their
  -- corresponding statements in the AST.
  let (linkedPF, parserReport) =
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

  modify $ \ s -> s { usConstraints = cons }

  debugLogging

-- Inference functions

-- | Return a list of critical variables as UnitInfo list (most likely
-- to be of the UnitVar constructor).
runCriticalVariables :: UnitSolver [UnitInfo]
runCriticalVariables = do
  cons <- usConstraints `fmap` get
  return $ criticalVariables cons

-- | Return a list of variable names mapped to their corresponding
-- unit that was inferred.
runInferVariables :: UnitSolver [(VV, UnitInfo)]
runInferVariables = do
  cons <- usConstraints `fmap` get
  return $ inferVariables cons

-- | Return a possible list of unsolvable constraints.
runInconsistentConstraints :: UnitSolver (Maybe Constraints)
runInconsistentConstraints = do
  cons <- usConstraints `fmap` get
  return $ inconsistentConstraints cons

-- | Produce information for a "units-mod" file.
runCompileUnits :: UnitSolver CompiledUnits
runCompileUnits = do
  cons <- usConstraints `fmap` get

  -- Sketching some ideas about solving the unit equation for each
  -- parameter of each function.
  let unitAssigns = map (fmap flattenUnits) $ genUnitAssignments cons
  let mulCons x = map (\ (UnitPow u k) -> UnitPow u (x * k))
  let negateCons = mulCons (-1)
  let epsilon = 0.001 -- arbitrary
  let approxEq a b = abs (b - a) < epsilon
  let uninvert ([UnitPow u k], rhs) | not (k `approxEq` 1) = ([UnitPow u 1], mulCons (1 / k) rhs)
      uninvert (lhs, rhs)                                  = (lhs, rhs)
  let shiftTerms name pos (lhs, rhs) = (lhsOk ++ negateCons rhsShift, rhsOk ++ negateCons lhsShift)
        where
          (lhsOk, lhsShift) = partition isLHS lhs
          (rhsOk, rhsShift) = partition (not . isLHS) rhs
          isLHS (UnitParamPosAbs (n, i)) | n == name && i == pos = True
          isLHS (UnitPow u _) = isLHS u
          isLHS _ = False

  let nameParams = M.fromList [ (NPKParam name pos, rhs) | assign <- unitAssigns
                                                         , UnitParamPosAbs (name, pos) <- universeBi assign
                                                         , let (_, rhs) = uninvert $ shiftTerms name pos assign ]


  let variables = M.fromList [ (NPKVariable var, units) | ([UnitPow (UnitVar var) k], units) <- unitAssigns
                                                        , k `approxEq` 1 ]

  tmap <- gets usTemplateMap
  return $ CompiledUnits { cuTemplateMap = tmap, cuNameParamMap = nameParams `M.union` variables }

--------------------------------------------------

-- | Seek out any parameters to functions or subroutines that do not
-- already have units, and insert parametric units for them into the
-- map of variables to UnitInfo.
insertParametricUnits :: UnitSolver ()
insertParametricUnits = gets usProgramFile >>= (mapM_ paramPU . universeBi)
  where
    paramPU pu = do
      forM_ (indexedParams pu) $ \ (i, param) -> do
        -- Insert a parametric unit if the variable does not already have a unit.
        modifyVarUnitMap $ M.insertWith (curry snd) param (UnitParamPosAbs (fname, i))
      where
        fname = (puName pu, puSrcName pu)

-- | Return the list of parameters paired with its positional index.
indexedParams :: F.ProgramUnit UA -> [(Int, VV)]
indexedParams pu
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
  pf   <- gets usProgramFile
  dmap <- (M.union (extractDeclMap pf) . combinedDeclMap . M.elems) `fmap` asks uoModFiles
  forM_ (universeBi pf :: [F.ProgramUnit UA]) $ \ pu ->
    modifyPUBlocksM (transformBiM (insertUndeterminedUnitVar dmap)) pu

-- Specifically handle variables
insertUndeterminedUnitVar :: DeclMap -> F.Expression UA -> UnitSolver (F.Expression UA)
insertUndeterminedUnitVar dmap v@(F.ExpValue _ _ (F.ValVariable _)) = do
  let vname = varName v
  let sname = srcName v
  let unit  = toUnitVar dmap (vname, sname)
  modifyVarUnitMap $ M.insertWith (curry snd) (varName v, srcName v) unit
  return v
insertUndeterminedUnitVar _ e = return e

-- Choose UnitVar or UnitParamVarAbs depending upon how the variable was declared.
toUnitVar :: DeclMap -> VV -> UnitInfo
toUnitVar dmap (vname, sname) = unit
  where
    unit = case fst `fmap` M.lookup vname dmap of
      Just (DCFunction (F.Named fvname, F.Named fsname))   -> UnitParamVarAbs ((fvname, fsname), (vname, sname))
      Just (DCSubroutine (F.Named fvname, F.Named fsname)) -> UnitParamVarAbs ((fvname, fsname), (vname, sname))
      _                                                    -> UnitVar (vname, sname)

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
  pf <- gets usProgramFile
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
      | otherwise                                 = return ()
      where
        mSpec = UA.unitSpec (FA.prevAnnotation a)
        mPU   = UA.unitPU   (FA.prevAnnotation a)
    -- Other type of ProgramUnit (e.g. one with a body of blocks)
    checkPU pu = mapM_ (checkBlockComment (getName pu)) [ b | b@(F.BlComment {}) <- universeBi (F.programUnitBody pu) ]
      where
        getName pu = case pu of
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
      | otherwise                                  = return ()
      where
        mSpec  = UA.unitSpec  (FA.prevAnnotation a)
        mBlock = UA.unitBlock (FA.prevAnnotation a)

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

--------------------------------------------------

-- | Take the unit information from the VarUnitMap and use it to
-- annotate every variable expression in the AST.
annotateAllVariables :: UnitSolver ()
annotateAllVariables = modifyProgramFileM $ \ pf -> do
  varUnitMap <- usVarUnitMap `fmap` get
  let annotateExp e@(F.ExpValue _ _ (F.ValVariable _))
        | Just info <- M.lookup (varName e, srcName e) varUnitMap = UA.setUnitInfo info e
      -- may need to annotate intrinsics separately
      annotateExp e = e
  return $ transformBi annotateExp pf

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
  mode <- asks uoLiterals
  case mode of
    LitUnitless -> modifyPUBlocksM (transformBiM expUnitless) pu
    LitPoly     -> modifyPUBlocksM (transformBiM (withLiterals genParamLit)) pu
    LitMixed    -> modifyPUBlocksM (transformBiM expMixed) pu
  where
    -- Follow the LitMixed rules.
    expMixed e = case e of
      F.ExpValue _ _ (F.ValInteger i) | readInteger i == Just 0 -> withLiterals genParamLit e
                                      | isPolyCtxt              -> expUnitless e
                                      | otherwise               -> withLiterals genUnitLiteral e
      F.ExpValue _ _ (F.ValReal i) | readReal i == Just 0       -> withLiterals genParamLit e
                                   | isPolyCtxt                 -> expUnitless e
                                   | otherwise                  -> withLiterals genUnitLiteral e
      _                                                         -> return e

    -- Set all literals to unitless.
    expUnitless e
      | isLiteral e = return $ UA.setUnitInfo UnitlessLit e
      | otherwise   = return e

    -- Set all literals to the result of given monadic computation.
    withLiterals m e
      | isLiteral e = flip UA.setUnitInfo e `fmap` m
      | otherwise   = return e

    isPolyCtxt = case pu of F.PUFunction {} -> True; F.PUSubroutine {} -> True; _ -> False

-- | Is it a literal, literally?
isLiteral :: F.Expression UA -> Bool
isLiteral (F.ExpValue _ _ (F.ValReal _))    = True
isLiteral (F.ExpValue _ _ (F.ValInteger _)) = True
isLiteral _                                 = False

-- | Is expression a literal and is it zero?
isLiteralNonZero :: F.Expression UA -> Bool
isLiteralNonZero (F.ExpValue _ _ (F.ValInteger i)) = readInteger i /= Just 0
isLiteralNonZero (F.ExpValue _ _ (F.ValReal i))    = readReal i    /= Just 0
isLiteralNonZero _                                 = False

--------------------------------------------------

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
  pf <- gets usProgramFile
  dummies <- forM (topLevelFuncsAndSubs pf) $ \ pu -> do
    id <- genCallId
    return (puName pu, id)

  whenDebug $ do
    D.traceM ("instances: " ++ show instances ++ "\n")
    D.traceM ("dummies: " ++ show dummies ++ "\n")

  -- Work through the instances, expanding their templates, and
  -- substituting the callId into the abstract parameters.
  concreteCons <- liftM2 (++) (foldM (substInstance False []) [] instances)
                              (foldM (substInstance True []) [] dummies)
  dumpConsM "applyTemplates: concreteCons" concreteCons

  -- Also include aliases in the final set of constraints, where
  -- aliases are implemented by simply asserting that they are equal
  -- to their definition.
  aliasMap <- usUnitAliasMap `fmap` get
  let aliases = [ ConEq (UnitAlias name) def | (name, def) <- M.toList aliasMap ]
  let transAlias (UnitName a) | a `M.member` aliasMap = UnitAlias a
      transAlias u                                    = u

  dumpConsM "aliases" aliases
  return . transformBi transAlias $ cons ++ concreteCons ++ aliases

-- | Look up the Parametric templates for a given function or
-- subroutine, and do the substitutions. Process any additional
-- polymorphic calls that are uncovered, unless they are recursive
-- calls that have already been seen in the current call stack.
substInstance :: Bool -> [F.Name] -> Constraints -> (F.Name, Int) -> UnitSolver Constraints
substInstance isDummy callStack output (name, callId) = do
  tmap <- gets usTemplateMap

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
  modify $ \ s -> s { usCallIdRemap = IM.empty }

  -- If any new instances are discovered, also process them, unless recursive.
  let instances = nub [ (name, i) | UnitParamPosUse ((name, _), _, i) <- universeBi template ]
  template' <- if name `elem` callStack then
                 -- Detected recursion: we do not support polymorphic-unit recursion,
                 -- ergo all subsequent recursive calls are assumed to have the same
                 -- unit-assignments as the first call.
                 return []
               else
                 foldM (substInstance False (name:callStack)) [] instances

  dumpConsM ("instantiating " ++ show (name, callId) ++ ": (output ++ template) is") (output ++ template)
  dumpConsM ("instantiating " ++ show (name, callId) ++ ": (template') is") (template')

  -- Get constraints for any imported variables
  let filterForVars (NPKVariable _) _ = True; filterForVars _ _ = False
  nmap <- M.filterWithKey filterForVars `fmap` gets usNameParamMap
  let importedVariables = [ ConEq (UnitVar vv) (foldUnits units) | (NPKVariable vv, units) <- M.toList nmap ]

  -- Convert abstract parametric units into concrete ones.

  let output' = -- Do not instantiate explicitly annotated polymorphic
                -- variables from current context when looking at dummy (name, callId)
                (if isDummy then output ++ template
                            else instantiate callId (output ++ template)) ++

                -- Only instantiate explicitly annotated polymorphic
                -- variables from nested function/subroutine calls.
                instantiate callId template' ++

                -- any imported variables
                importedVariables

  dumpConsM ("final output for " ++ show (name, callId)) output'

  return output'

foldUnits units
  | null units = UnitlessVar
  | otherwise  = foldl1 UnitMul units

-- | Generate constraints from a NameParamMap entry.
nameParamConstraints :: F.Name -> UnitSolver Constraints
nameParamConstraints fname = do
  let filterForName (NPKParam (n, _) _) _ = n == fname
      filterForName _ _                   = False
  nlst <- (M.toList . M.filterWithKey filterForName) `fmap` gets usNameParamMap
  return [ ConEq (UnitParamPosAbs (n, pos)) (foldUnits units) | (NPKParam n pos, units) <- nlst ]

-- | If given a usage of a parametric unit, rewrite the callId field
-- to follow an existing mapping in the usCallIdRemap state field, or
-- generate a new callId and add it to the usCallIdRemap state field.
callIdRemap :: UnitInfo -> UnitSolver UnitInfo
callIdRemap info = modifyCallIdRemapM $ \ idMap -> case info of
    UnitParamPosUse (n, p, i)
      | Just i' <- IM.lookup i idMap -> return (UnitParamPosUse (n, p, i'), idMap)
      | otherwise                    -> genCallId >>= \ i' ->
                                          return (UnitParamPosUse (n, p, i'), IM.insert i i' idMap)
    UnitParamVarUse (n, v, i)
      | Just i' <- IM.lookup i idMap -> return (UnitParamVarUse (n, v, i'), idMap)
      | otherwise                    -> genCallId >>= \ i' ->
                                          return (UnitParamVarUse (n, v, i'), IM.insert i i' idMap)
    UnitParamLitUse (l, i)
      | Just i' <- IM.lookup i idMap -> return (UnitParamLitUse (l, i'), idMap)
      | otherwise                    -> genCallId >>= \ i' ->
                                          return (UnitParamLitUse (l, i'), IM.insert i i' idMap)
    UnitParamEAPUse (v, i)
      | Just i' <- IM.lookup i idMap -> return (UnitParamEAPUse (v, i'), idMap)
      | otherwise                    -> genCallId >>= \ i' ->
                                          return (UnitParamEAPUse (v, i'), IM.insert i i' idMap)

    _                                -> return (info, idMap)


-- | Convert a parametric template into a particular use.
instantiate :: Data a => Int -> a -> a
instantiate callId = transformBi $ \ info -> case info of
  UnitParamPosAbs (name, position) -> UnitParamPosUse (name, position, callId)
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
    topLevel f@(F.PUFunction {})                  = return f
    topLevel s@(F.PUSubroutine {})                = return s
    topLevel _                                    = []

--------------------------------------------------

-- | Gather all constraints from the main blocks of the AST, as well as from the varUnitMap
extractConstraints :: UnitSolver Constraints
extractConstraints = do
  pf         <- gets usProgramFile
  dmap       <- (M.union (extractDeclMap pf) . combinedDeclMap . M.elems) `fmap` asks uoModFiles
  varUnitMap <- gets usVarUnitMap
  return $ [ con | b <- mainBlocks pf, con@(ConEq {}) <- universeBi b ] ++
           [ ConEq (toUnitVar dmap v) u | (v, u) <- M.toList varUnitMap ]

-- | A list of blocks considered to be part of the 'main' program.
mainBlocks :: F.ProgramFile UA -> [F.Block UA]
mainBlocks = concatMap getBlocks . universeBi
  where
    getBlocks (F.PUMain _ _ _ bs _)   = bs
    getBlocks (F.PUModule _ _ _ bs _) = bs
    getBlocks _                       = []

--------------------------------------------------

-- | Extract the unit info from a given annotated piece of AST, within
-- the context of a multiplication expression, and given a particular
-- mode for handling literals.
--
-- The point is that the unit-assignment of a literal constant can
-- vary depending upon whether it is being multiplied by a variable
-- with units, and possibly by global options that assume one way or
-- the other.
getUnitInfoMul :: LiteralsOpt -> F.Expression UA -> Maybe UnitInfo
getUnitInfoMul LitPoly e          = UA.getUnitInfo e
getUnitInfoMul _ e
  | isJust (constantExpression e) = Just UnitlessLit
  | otherwise                     = UA.getUnitInfo e


-- | Decorate the AST with unit info.
propagateUnits :: UnitSolver ()
-- precondition: all variables have already been annotated
propagateUnits = modifyProgramFileM $ transformBiM propagatePU        <=<
                                      transformBiM propagateStatement <=<
                                      transformBiM propagateExp

propagateExp :: F.Expression UA -> UnitSolver (F.Expression UA)
propagateExp e = fmap uoLiterals ask >>= \ lm -> case e of
  F.ExpValue _ _ _                       -> return e -- all values should already be annotated
  F.ExpBinary _ _ F.Multiplication e1 e2 -> setF2 UnitMul (getUnitInfoMul lm e1) (getUnitInfoMul lm e2)
  F.ExpBinary _ _ F.Division e1 e2       -> setF2 UnitMul (getUnitInfoMul lm e1) (flip UnitPow (-1) `fmap` (getUnitInfoMul lm e2))
  F.ExpBinary _ _ F.Exponentiation e1 e2 -> setF2 UnitPow (UA.getUnitInfo e1) (constantExpression e2)
  F.ExpBinary _ _ o e1 e2 | isOp AddOp o -> setF2C ConEq  (UA.getUnitInfo e1) (UA.getUnitInfo e2)
                          | isOp RelOp o -> setF2C ConEq  (UA.getUnitInfo e1) (UA.getUnitInfo e2)
  F.ExpFunctionCall {}                   -> propagateFunctionCall e
  F.ExpSubscript _ _ e1 _                -> return $ UA.maybeSetUnitInfo (UA.getUnitInfo e1) e
  F.ExpUnary _ _ _ e1                    -> return $ UA.maybeSetUnitInfo (UA.getUnitInfo e1) e
  _                                      -> do
    whenDebug . tell $ "propagateExp: " ++ show (getSpan e) ++ " unhandled: " ++ show e
    return e
  where
    -- Shorter names for convenience functions.
    setF2 f u1 u2  = return $ UA.maybeSetUnitInfoF2 f u1 u2 e
    -- Remember, not only set a constraint, but also give a unit!
    setF2C f u1 u2 = return . UA.maybeSetUnitInfo u1 $ UA.maybeSetUnitConstraintF2 f u1 u2 e

propagateFunctionCall :: F.Expression UA -> UnitSolver (F.Expression UA)
propagateFunctionCall (F.ExpFunctionCall a s f Nothing)                     = do
  (info, _) <- callHelper f []
  let cons = intrinsicHelper info f []
  return . UA.setConstraint (ConConj cons) . UA.setUnitInfo info $ F.ExpFunctionCall a s f Nothing
propagateFunctionCall (F.ExpFunctionCall a s f (Just (F.AList a' s' args))) = do
  (info, args') <- callHelper f args
  let cons = intrinsicHelper info f args'
  return . UA.setConstraint (ConConj cons) . UA.setUnitInfo info $ F.ExpFunctionCall a s f (Just (F.AList a' s' args'))

propagateStatement :: F.Statement UA -> UnitSolver (F.Statement UA)
propagateStatement stmt = case stmt of
  F.StExpressionAssign _ _ e1 e2               -> literalAssignmentSpecialCase e1 e2 stmt
  F.StCall a s sub (Just (F.AList a' s' args)) -> do
    (info, args') <- callHelper sub args
    let cons = intrinsicHelper info sub args'
    return . UA.setConstraint (ConConj cons) $ F.StCall a s sub (Just (F.AList a' s' args'))
  F.StDeclaration {}                           -> transformBiM propagateDeclarator stmt
  _                                            -> return stmt

propagateDeclarator :: F.Declarator UA -> UnitSolver (F.Declarator UA)
propagateDeclarator decl = case decl of
  F.DeclVariable _ _ e1 _ (Just e2) -> literalAssignmentSpecialCase e1 e2 decl
  F.DeclArray _ _ e1 _ _ (Just e2)  -> literalAssignmentSpecialCase e1 e2 decl
  _                                 -> return decl

-- Allow literal assignment to overload the non-polymorphic
-- unit-assignment of the non-zero literal.
literalAssignmentSpecialCase e1 e2 ast
  | u2@(Just (UnitLiteral _)) <- UA.getUnitInfo e2 = do
    return $ UA.maybeSetUnitConstraintF2 ConEq (UA.getUnitInfo e1) u2 ast
  | isLiteralNonZero e2                         = do
    u2 <- genUnitLiteral
    return $ UA.maybeSetUnitConstraintF2 ConEq (UA.getUnitInfo e1) (Just u2) ast
  | otherwise                                   = do
    -- otherwise express the constraint between LHS and RHS of assignment.
    return $ UA.maybeSetUnitConstraintF2 ConEq (UA.getUnitInfo e1) (UA.getUnitInfo e2) ast

propagatePU :: F.ProgramUnit UA -> UnitSolver (F.ProgramUnit UA)
propagatePU pu = do
  let name     = puName pu
  let sname    = puSrcName pu
  let nn       = (name, sname)
  let bodyCons = [ con | con@(ConEq {}) <- universeBi pu ] -- Constraints within the PU.

  varMap <- gets usVarUnitMap

  -- If any of the function/subroutine parameters was given an
  -- explicit unit annotation, then create a constraint between that
  -- explicit unit and the UnitParamPosAbs corresponding to the
  -- parameter. This way all other uses of the parameter get linked to
  -- the explicit unit annotation as well.
  givenCons <- forM (indexedParams pu) $ \ (i, param) -> do
    case M.lookup param varMap of
      Just (UnitParamPosAbs {}) -> return . ConEq (UnitParamVarAbs (nn, param)) $ UnitParamPosAbs (nn, i)
      Just u                    -> return . ConEq u $ UnitParamPosAbs (nn, i)
      _                         -> return . ConEq (UnitParamVarAbs (nn, param)) $ UnitParamPosAbs (nn, i)

  let cons = givenCons ++ bodyCons
  case pu of F.PUFunction {}   -> modifyTemplateMap (M.insert name cons)
             F.PUSubroutine {} -> modifyTemplateMap (M.insert name cons)
             _                 -> return ()

  -- Set the unitInfo field of a function program unit to be the same
  -- as the unitInfo of its result.
  let pu' = case (pu, indexedParams pu) of
              (F.PUFunction {}, (0, res):_) -> UA.setUnitInfo (UnitParamPosAbs (nn, 0) `fromMaybe` M.lookup res varMap) pu
              _                             -> pu

  return (UA.setConstraint (ConConj cons) pu')

--------------------------------------------------

-- | Coalesce various function and subroutine call common code.
callHelper :: F.Expression UA -> [F.Argument UA] -> UnitSolver (UnitInfo, [F.Argument UA])
callHelper nexp args = do
  let name = (varName nexp, srcName nexp)
  callId <- genCallId -- every call-site gets its own unique identifier
  let eachArg i arg@(F.Argument _ _ _ e)
        -- add site-specific parametric constraints to each argument
        | Just u <- UA.getUnitInfo e = UA.setConstraint (ConEq u (UnitParamPosUse (name, i, callId))) arg
        | otherwise               = arg
  let args' = zipWith eachArg [1..] args
  -- build a site-specific parametric unit for use on a return variable, if any
  let info = UnitParamPosUse (name, 0, callId)
  return (info, args')

-- FIXME: use this function to create a list of constraints on intrinsic call-sites...
intrinsicHelper (UnitParamPosUse (_, _, callId)) f@(F.ExpValue _ _ (F.ValIntrinsic _)) args
  | Just (retU, argUs) <- M.lookup sname intrinsicUnits = zipWith eachArg [0..numArgs] (retU:argUs)
  where
    numArgs     = length args
    sname       = srcName f
    vname       = varName f
    eachArg i u = ConEq (UnitParamPosUse ((vname, sname), i, callId)) (instantiate callId u)
intrinsicHelper _ _ _ = []

-- | Generate a unique identifier for a call-site.
genCallId :: UnitSolver Int
genCallId = do
  st <- get
  let callId = usCallIds st
  put $ st { usCallIds = callId + 1 }
  return callId

-- | Generate a unique identifier for a literal encountered in the code.
genUnitLiteral :: UnitSolver UnitInfo
genUnitLiteral = do
  s <- get
  let i = usLitNums s
  put $ s { usLitNums = i + 1 }
  return $ UnitLiteral i

-- | Generate a unique identifier for a polymorphic literal encountered in the code.
genParamLit :: UnitSolver UnitInfo
genParamLit = do
  s <- get
  let i = usLitNums s
  put $ s { usLitNums = i + 1 }
  return $ UnitParamLitAbs i

-- Operate only on the blocks of a program unit, not the contained sub-programunits.
modifyPUBlocksM :: Monad m => ([F.Block a] -> m [F.Block a]) -> F.ProgramUnit a -> m (F.ProgramUnit a)
modifyPUBlocksM f pu = case pu of
  F.PUMain a s n b pus                    -> flip fmap (f b) $ \ b' -> F.PUMain a s n b' pus
  F.PUModule a s n b pus                  -> flip fmap (f b) $ \ b' -> F.PUModule a s n b' pus
  F.PUSubroutine a s r n p b subs         -> flip fmap (f b) $ \ b' -> F.PUSubroutine a s r n p b' subs
  F.PUFunction   a s r rec n p res b subs -> flip fmap (f b) $ \ b' -> F.PUFunction a s r rec n p res b' subs
  F.PUBlockData  a s n b                  -> flip fmap (f b) $ \ b' -> F.PUBlockData  a s n b'
  F.PUComment {}                          -> return pu -- no blocks

-- Fortran semantics for interpretation of constant expressions
-- involving numeric literals.
data FNum = FReal Double | FInt Integer
fnumToDouble (FReal x) = x
fnumToDouble (FInt x)  = fromIntegral x

fAdd, fSub, fMul, fDiv :: FNum -> FNum -> FNum
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

fDivMaybe mx my
  | Just y <- my,
    fnumToDouble y == 0.0 = Nothing
  | otherwise             = liftM2 fDiv mx my

-- | Statically computes if the expression is a constant value.
constantExpression :: F.Expression a -> Maybe Double
constantExpression e = fnumToDouble `fmap` ce e
  where
    ce e = case e of
      (F.ExpValue _ _ (F.ValInteger i))        -> FInt `fmap` readInteger i
      (F.ExpValue _ _ (F.ValReal r))           -> FReal `fmap` readReal r
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
binOpKind F.Equivalent       = RelOp
binOpKind F.NotEquivalent    = RelOp
binOpKind (F.BinCustom _)    = RelOp

--------------------------------------------------

dumpConsM str = whenDebug . D.traceM . unlines . ([replicate 50 '-', str ++ ":"]++) . (++[replicate 50 '^']) . map f
  where
    f (ConEq u1 u2)  = show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2)
    f (ConConj cons) = intercalate " && " (map f cons)

debugLogging :: UnitSolver ()
debugLogging = whenDebug $ do
    (tell . unlines . map (\ (ConEq u1 u2) -> "  ***AbsConstraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2) ++ "\n")) =<< extractConstraints
    pf <- gets usProgramFile
    cons <- usConstraints `fmap` get
    vum <- usVarUnitMap `fmap` get
    tell . unlines $ [ "  " ++ show info ++ " :: " ++ n | ((n, _), info) <- M.toList vum ]
    tell "\n\n"
    uam <- usUnitAliasMap `fmap` get
    tell . unlines $ [ "  " ++ n ++ " = " ++ show info | (n, info) <- M.toList uam ]
    tell . unlines $ map (\ (ConEq u1 u2) -> "  ***Constraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2) ++ "\n") cons
    tell $ show cons ++ "\n\n"
    forM_ (universeBi pf) $ \ pu -> case pu of
      F.PUFunction {}
        | Just (ConConj cons) <- UA.getConstraint pu ->
          tell . unlines $ (puName pu ++ ":"):map (\ (ConEq u1 u2) -> "    constraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2)) cons
      F.PUSubroutine {}
        | Just (ConConj cons) <- UA.getConstraint pu ->
          tell . unlines $ (puName pu ++ ":"):map (\ (ConEq u1 u2) -> "    constraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2)) cons
      _ -> return ()
    let (lhsM, rhsM, _, lhsColA, rhsColA) = constraintsToMatrices cons
    tell "\n--------------------------------------------------\nLHS Cols:\n"
    tell $ show lhsColA
    tell "\n--------------------------------------------------\nRHS Cols:\n"
    tell $ show rhsColA
    tell "\n--------------------------------------------------\nLHS M:\n"
    tell $ show lhsM
    tell "\n--------------------------------------------------\nRHS M:\n"
    tell $ show rhsM
    tell "\n--------------------------------------------------\nSolved (RREF) M:\n"
    let augM = if H.rows rhsM == 0 || H.cols rhsM == 0 then lhsM else H.fromBlocks [[lhsM, rhsM]]
    tell . show . rref $ augM
    -- tell "\n--------------------------------------------------\nSolved (SVD) M:\n"
    -- tell $ show (H.linearSolveSVD lhsM rhsM)
    -- tell "\n--------------------------------------------------\nSingular Values:\n"
    -- tell $ show (H.singularValues lhsM)
    tell "\n--------------------------------------------------\n"
    tell $ "Rank LHS: " ++ show (H.rank lhsM) ++ "\n"
    tell "\n--------------------------------------------------\n"
    let augA = if H.rows rhsM == 0 || H.cols rhsM == 0 then lhsM else H.fromBlocks [[lhsM, rhsM]]
    tell $ "Rank Augmented: " ++ show (H.rank augA) ++ "\n"
    tell "\n--------------------------------------------------\nGenUnitAssignments:\n"
    let unitAssignments = genUnitAssignments cons
    tell . unlines $ map (\ (u1s, u2) -> "  ***UnitAssignment: " ++ show u1s ++ " === " ++ show (flattenUnits u2) ++ "\n") unitAssignments
    tell "\n--------------------------------------------------\n"

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

-- | name => (return-unit, parameter-units)
intrinsicUnits :: M.Map F.Name (UnitInfo, [UnitInfo])
intrinsicUnits =
  M.fromList
    [ ("abs", (UnitParamEAPAbs ("'a", "'a"), [UnitParamEAPAbs ("'a", "'a")]))
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
