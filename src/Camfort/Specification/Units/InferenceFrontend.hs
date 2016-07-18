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

  Units of measure extension to Fortran

TODO:
 * Deal with variable shadowing in "contained" functions.
 * Better errors with line number info

-}


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Camfort.Specification.Units.InferenceFrontend (doInferUnits, solveProgramFile) where

import Data.Data
import Data.Char
import Data.Function
import Data.List
import Data.Matrix
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Data.Generics.Uniplate.Operations
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Writer.Strict
import Control.Monad.RWS.Strict hiding (gets)
import GHC.Prim

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Fortran.Util.SecondParameter as FUS

import Camfort.Analysis.CommentAnnotator
import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Analysis.Types
import Camfort.Specification.Units.Debug
import Camfort.Specification.Units.Monad
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.InferenceBackend
import Camfort.Specification.Units.Solve
import qualified Camfort.Specification.Units.Parser as Parser
import qualified Camfort.Specification.Units.Parser as P
import Camfort.Transformation.Syntax

import qualified Debug.Trace as D
import qualified Numeric.LinearAlgebra as H

--------------------------------------------------

type UA = FA.Analysis (UnitAnnotation A)
-- Instances for embedding parsed specifications into the AST
instance ASTEmbeddable UA P.UnitStatement where
  annotateWithAST ann ast =
    onPrev (\ann -> ann { unitSpec = Just ast }) ann

-- Link annotation comments to declaration statements
instance Linkable UA where
  link ann (b@(F.BlStatement _ _ _ (F.StDeclaration {}))) =
      onPrev (\ann -> ann { unitBlock = Just b }) ann
  link ann b = ann

-- Helper for transforming the 'previous' annotation
onPrev :: (a -> a) -> FA.Analysis a -> FA.Analysis a
onPrev f ann = ann { FA.prevAnnotation = f (FA.prevAnnotation ann) }

modifyAnnotation :: F.Annotated f => (a -> a) -> f a -> f a
modifyAnnotation f x = F.setAnnotation (f (F.getAnnotation x)) x

--------------------------------------------------

solveProgramFile :: F.ProgramFile UA -> UnitSolver ()
solveProgramFile pf = do
  -- Parse unit annotations found in comments and link to their
  -- corresponding statements in the AST.
  let (linkedPF, parserReport) = runWriter $ annotateComments P.unitParser pf
  -- Send the output of the parser to the logger.
  mapM_ tell parserReport

  insertGivenUnits linkedPF -- also obtains all unit alias definitions
  insertParametricUnits linkedPF
  insertUndeterminedUnits linkedPF
  annotPF <- annotateAllVariables linkedPF

  -- (FIXME: is it necessary?) repeat until fixed point:
  propPF <- propagateUnits annotPF
  consWithoutTemplates <- extractConstraints propPF
  cons <- applyTemplates consWithoutTemplates
  -- end repeat

  debug <- uoDebug `fmap` ask
  when debug $ do
    vum <- usVarUnitMap `fmap` get
    tell . unlines $ [ "  " ++ show info ++ " :: " ++ n | (n, info) <- M.toList vum ]
    tell "\n\n"
    uam <- usUnitAliasMap `fmap` get
    tell . unlines $ [ "  " ++ n ++ " = " ++ show info | (n, info) <- M.toList uam ]
    tell . unlines $ map (\ (UnitEq u1 u2) -> "  ***Constraint: " ++ show (flattenUnits u1) ++ " === " ++ show (flattenUnits u2) ++ "\n") cons
    tell $ show cons
    let (unsolvedM, inconsists, colA) = constraintsToMatrix cons
    let solvedM = rref unsolvedM
    tell "\n--------------------------------------------------\n"
    tell $ show colA
    tell "\n--------------------------------------------------\n"
    tell $ show unsolvedM
    tell "\n--------------------------------------------------\n"
    tell . show $ (H.takeRows (H.rank solvedM) solvedM)
    tell "\n--------------------------------------------------\n"
    tell $ "Rank: " ++ show (H.rank solvedM) ++ "\n"
    tell $ "Is inconsistent RREF? " ++ show (isInconsistentRREF solvedM) ++ "\n"
    tell $ "Inconsistent rows: " ++ show (inconsistentConstraints cons) ++ "\n"
    tell "--------------------------------------------------\n"
    tell $ "Critical Variables: " ++ show (criticalVariables cons) ++ "\n"
    tell $ "Infer Variables: " ++ show (inferVariables cons) ++ "\n"
  return ()

--------------------------------------------------

puName :: F.ProgramUnit UA -> F.Name
puName pu
  | F.Named n <- FA.puName pu = n
  | otherwise               = "_nameless"

varName :: F.Expression UA -> F.Name
varName = FA.varName

setUnitInfo :: F.Annotated f => UnitInfo -> f UA -> f UA
setUnitInfo info = modifyAnnotation (onPrev (\ ua -> ua { unitInfo = Just info }))

--------------------------------------------------

-- Seek out any parameters to functions or subroutines that do not
-- already have units, and insert parametric units for them into the
-- map of variables to UnitInfo.
insertParametricUnits :: F.ProgramFile UA -> UnitSolver ()
insertParametricUnits = mapM_ paramPU . universeBi
  where
    paramPU pu = do
      forM_ indexedParams $ \ (i, param) -> do
        -- Insert a parametric unit if the variable does not already have a unit.
        modifyVarUnitMap $ M.insertWith (curry snd) param (Parametric (fname, i))
      where
        fname = puName pu
        indexedParams
          | F.PUFunction _ _ _ _ _ (Just paList) (Just r) _ _ <- pu = zip [0..] $ varName r : map varName (F.aStrip paList)
          | F.PUFunction _ _ _ _ _ (Just paList) _ _ _        <- pu = zip [0..] $ fname     : map varName (F.aStrip paList)
          | F.PUSubroutine _ _ _ _ (Just paList) _ _          <- pu = zip [1..] $ map varName (F.aStrip paList)
          | otherwise                                               = []

--------------------------------------------------

-- Any remaining variables with unknown units are given unit
-- Undetermined with a unique name (in this case, taken from the
-- unique name of the variable as provided by the Renamer).
insertUndeterminedUnits :: F.ProgramFile UA -> UnitSolver ()
insertUndeterminedUnits pf = forM_ (nub [ varName v | v@(F.ExpValue _ _ (F.ValVariable _)) <- universeBi pf ]) $ \ vname ->
  -- Insert an undetermined unit if the variable does not already have a unit.
  modifyVarUnitMap $ M.insertWith (curry snd) vname (Undetermined vname)

--------------------------------------------------

-- Any units provided by the programmer through comment annotations
-- will be incorporated into the VarUnitMap.
insertGivenUnits :: F.ProgramFile UA -> UnitSolver ()
insertGivenUnits pf = mapM_ checkComment [ b | b@(F.BlComment {}) <- universeBi pf ]
  where
    -- Look through each comment that has some kind of unit annotation within it.
    checkComment :: F.Block UA -> UnitSolver ()
    checkComment (F.BlComment a _ _)
      -- Look at unit assignment between variable and spec.
      | Just (P.UnitAssignment (Just vars) unitsAST) <- mSpec
      , Just b                                       <- mBlock = insertUnitAssignments (toUnitInfo unitsAST) b vars
      -- Add a new unit alias.
      | Just (P.UnitAlias name unitsAST)             <- mSpec  = modifyUnitAliasMap (M.insert name (toUnitInfo unitsAST))
      | otherwise                                              = return ()
      where
        mSpec  = unitSpec (FA.prevAnnotation a)
        mBlock = unitBlock (FA.prevAnnotation a)

    -- Figure out the unique names of the referenced variables and
    -- then insert unit info under each of those names.
    insertUnitAssignments info (F.BlStatement _ _ _ (F.StDeclaration _ _ _ _ decls)) varRealNames = do
      -- figure out the 'unique name' of the varRealName that was found in the comment
      -- FIXME: account for module renaming
      -- FIXME: might be more efficient to allow access to variable renaming environ at this program point
      nameMap <- uoNameMap `fmap` ask
      let m = M.fromList [ (varUniqueName, info) | e@(F.ExpValue _ _ (F.ValVariable _)) <- universeBi decls
                                                 , varRealName <- varRealNames
                                                 , let varUniqueName = varName e
                                                 , maybe False (== varRealName) (varUniqueName `M.lookup` nameMap) ]
      modifyVarUnitMap $ M.unionWith const m

--------------------------------------------------

-- Take the unit information from the VarUnitMap and use it to
-- annotate every variable expression in the AST.
annotateAllVariables :: F.ProgramFile UA -> UnitSolver (F.ProgramFile UA)
annotateAllVariables pf = do
  varUnitMap <- usVarUnitMap `fmap` get
  let annotateExp e@(F.ExpValue _ _ (F.ValVariable _))
        | Just info <- M.lookup (varName e) varUnitMap = setUnitInfo info e
      annotateExp e = e
  return $ transformBi annotateExp pf

--------------------------------------------------

-- Convert all parametric templates into actual uses, via substitution
applyTemplates :: Constraints -> UnitSolver Constraints
-- postcondition: returned constraints lack all Parametric constructors
applyTemplates cons = do
  let instances = nub [ (name, i) | ParametricUse (name, _, i) <- universeBi cons ]
  temps <- foldM substInstance [] instances
  aliasMap <- usUnitAliasMap `fmap` get
  let aliases = [ UnitEq (UnitAlias name) def | (name, def) <- M.toList aliasMap ]
  let transAlias (UnitName a) | a `M.member` aliasMap = UnitAlias a
      transAlias u                                    = u
  return . transformBi transAlias . filter (not . isParametric) $ cons ++ temps ++ aliases

-- look up Parametric template and apply it to everything
substInstance :: [UnitInfo] -> (F.Name, Int) -> UnitSolver [UnitInfo]
substInstance output (name, callId) = do
  tmap <- usTemplateMap `fmap` get
  return . instantiate (name, callId) . (output ++) $ [] `fromMaybe` M.lookup name tmap

-- Convert a parametric template into a particular use
instantiate (name, callId) = transformBi $ \ info -> case info of
  Parametric (name, position) -> ParametricUse (name, position, callId)
  _                           -> info

-- Gather all constraints from the AST, as well as from the varUnitMap
extractConstraints :: F.ProgramFile UA -> UnitSolver Constraints
extractConstraints pf = do
  varUnitMap <- usVarUnitMap `fmap` get
  return $ [ info | info@(UnitEq {}) <- universeBi pf ] ++
           [ UnitEq (Determined v) u | (v, u) <- M.toList varUnitMap ]

-- Does the UnitInfo contain any Parametric elements?
isParametric :: UnitInfo -> Bool
isParametric info = not (null [ () | Parametric _ <- universeBi info ])

--------------------------------------------------

-- Decorate the AST with unit info
propagateUnits :: F.ProgramFile UA -> UnitSolver (F.ProgramFile UA)
-- precondition: all variables have already been annotated
propagateUnits = transformBiM propagatePU <=< transformBiM propagateStatement <=< transformBiM propagateExp

propagateExp :: F.Expression UA -> UnitSolver (F.Expression UA)
propagateExp e = case e of
  F.ExpValue _ _ (F.ValVariable _)       -> return e -- all variables should already be annotated
  F.ExpValue _ _ (F.ValInteger _)        -> flip setUnitInfo e `fmap` genUndeterminedLit
  F.ExpValue _ _ (F.ValReal _)           -> flip setUnitInfo e `fmap` genUndeterminedLit
  F.ExpBinary _ _ F.Multiplication e1 e2 -> setF2 UnitMul (getUnitInfo e1) (getUnitInfo e2)
  F.ExpBinary _ _ F.Division e1 e2       -> setF2 UnitMul (getUnitInfo e1) (flip UnitPow (-1) `fmap` getUnitInfo e2)
  F.ExpBinary _ _ F.Addition e1 e2       -> setF2 UnitEq  (getUnitInfo e1) (getUnitInfo e2)
  F.ExpBinary _ _ F.Subtraction e1 e2    -> setF2 UnitEq  (getUnitInfo e1) (getUnitInfo e2)
  F.ExpBinary _ _ F.Exponentiation e1 e2 -> setF2 UnitPow (getUnitInfo e1) (constantExpression e2)
  F.ExpFunctionCall {}                   -> propagateFunctionCall e
  -- FIXME: relational ops
  _                                      -> tell ("propagateExp: unhandled: " ++ show e) >> return e
  where
    setF2 f u1 u2 = return $ maybeSetUnitInfoF2 f u1 u2 e

propagateFunctionCall :: F.Expression UA -> UnitSolver (F.Expression UA)
propagateFunctionCall e@(F.ExpFunctionCall a s f Nothing)                     = do
  (info, _)     <- callHelper f []
  return . setUnitInfo info $ F.ExpFunctionCall a s f Nothing
propagateFunctionCall e@(F.ExpFunctionCall a s f (Just (F.AList a' s' args))) = do
  (info, args') <- callHelper f args
  return . setUnitInfo info $ F.ExpFunctionCall a s f (Just (F.AList a' s' args'))

propagateStatement :: F.Statement UA -> UnitSolver (F.Statement UA)
propagateStatement stmt = case stmt of
  F.StExpressionAssign _ _ e1 e2               -> do
    return $ maybeSetUnitInfoF2 UnitEq (getUnitInfo e1) (getUnitInfo e2) stmt
  F.StCall a s sub (Just (F.AList a' s' args)) -> do
    (_, args') <- callHelper sub args
    return $ F.StCall a s sub (Just (F.AList a' s' args'))
  _                                            -> return stmt

propagatePU :: F.ProgramUnit UA -> UnitSolver (F.ProgramUnit UA)
propagatePU pu = modifyTemplateMap (M.insert name cons) >> return (setUnitInfo (UnitConj cons) pu)
  where
    cons = [ con | con@(UnitEq {}) <- universeBi pu, containsParametric name con ]
    name = puName pu

containsParametric :: Data from => String -> from -> Bool
containsParametric name x = not (null [ () | Parametric (name', _) <- universeBi x, name == name' ])

callHelper :: F.Expression UA -> [F.Argument UA] -> UnitSolver (UnitInfo, [F.Argument UA])
callHelper nexp args = do
  let name = varName nexp
  callId <- genCallId
  let eachArg i arg@(F.Argument _ _ _ e)
        | Just u <- getUnitInfo e = setUnitInfo (UnitEq u (ParametricUse (name, i, callId))) arg
        | otherwise               = arg
  let args' = zipWith eachArg [1..] args

  let info = ParametricUse (name, 0, callId)
  return (info, args')

genCallId :: UnitSolver Int
genCallId = do
  st <- get
  let callId = usLitNums st
  put $ st { usLitNums = callId + 1 }
  return callId

genUndeterminedLit :: UnitSolver UnitInfo
genUndeterminedLit = do
  s <- get
  let i = usLitNums s
  put $ s { usLitNums = i + 1 }
  return $ UndeterminedLit i

getUnitInfo :: F.Annotated f => f UA -> Maybe UnitInfo
getUnitInfo = fmap flattenUnitEq . unitInfo . FA.prevAnnotation . F.getAnnotation

flattenUnitEq :: UnitInfo -> UnitInfo
flattenUnitEq (UnitEq u _) = u
flattenUnitEq u            = u

maybeSetUnitInfo :: F.Annotated f => Maybe UnitInfo -> f UA -> f UA
maybeSetUnitInfo Nothing e = e
maybeSetUnitInfo (Just u) e = setUnitInfo u e

maybeSetUnitInfoF2 :: F.Annotated f => (a -> b -> UnitInfo) -> Maybe a -> Maybe b -> f UA -> f UA
maybeSetUnitInfoF2 f (Just u1) (Just u2) e = setUnitInfo (f u1 u2) e
maybeSetUnitInfoF2 _ _ _ e = e

constantExpression :: F.Expression a -> Maybe Double
constantExpression (F.ExpValue _ _ (F.ValInteger i)) = Just $ read i
constantExpression (F.ExpValue _ _ (F.ValReal r))    = Just $ read r
-- FIXME: expand...
constantExpression _                                 = Nothing

-- assertCompatible :: Maybe UnitInfo -> Maybe UnitInfo -> UnitSolver (Maybe UnitInfo)
-- assertCompatible Nothing (Just u)    = return $ Just u
-- assertCompatible (Just u) Nothing    = return $ Just u
-- assertCompatible (Just u1) (Just u2) = assertCompatible' u1 u2
-- assertCompatible _ _                 = return Nothing

-- assertCompatible' u1 u2 = case (u1, u2) of
--   (UnitName n1, UnitName n2) | n1 == n2 -> return $ Just u1
--   _ -> throwE (UEIncompatible u1 u2)


-- instead, annotate with constraint
-- x + y means that unit(x) = unit(y)
-- x = y means that unit(x) = unit(y)
-- x = f(y) means that unit(x) = unit(f_use,0) and unit(y) = unit(f_use,1)



--------------------------------------------------
--------------------------------------------------

type A1 = FA.Analysis (UnitAnnotation A)
type Params = (?criticals      :: Bool,
               ?solver         :: Solver,
               ?debug          :: Bool,
               ?assumeLiterals :: AssumeLiterals,
               ?nameMap        :: FAR.NameMap,
               ?argumentDecls  :: Bool)

realName :: (?nameMap :: FAR.NameMap) => F.Name -> F.Name
realName v = v `fromMaybe` (v `M.lookup` ?nameMap)
plumb f x = do { f x ; return x }



-- Core procedure for inferring units
doInferUnits ::
    Params => F.ProgramFile A1
           -> State UnitEnv ()
doInferUnits pf = do
    (pf', parserReport) <- return $ runWriter (annotateComments Parser.unitParser pf)
    report <<++ (intercalate "\n" parserReport)
    let ?argumentDecls = False in descendBiM perProgramUnit pf'
    --
    ifDebug (report <<++ "Finished inferring prog units")
    ifDebug debugGaussian
    --
    inferInterproceduralUnits pf'
    return ()

-- ***************************************
--
-- *  Unit inference (main, over all AST)
--
-- ***************************************

-- Check units per program unit, with special handling of functions and subroutines
-- which need adding to the set of constraints
perProgramUnit :: Params
    => F.ProgramUnit A1
    -> State UnitEnv (F.ProgramUnit A1)
perProgramUnit p@(F.PUMain _ _ _ body subprogs) = do
    resetTemps
    descendBiM perBlock body
    descendBiM perProgramUnit subprogs
    return p

perProgramUnit p@(F.PUModule _ _ _ body subprogs) = do
    resetTemps
    puname =: (Just $ FA.puName p)
    descendBiM perBlock p
    descendBiM perProgramUnit subprogs
    return p

perProgramUnit p@(F.PUSubroutine ann span rec name args body subprogs) = do
    resetTemps
    addProcedure rec name Nothing args body span
    descendBiM perProgramUnit subprogs
    return p

perProgramUnit p@(F.PUFunction ann span retTy rec name args result
                                                     body subprogs) = do
    resetTemps
    addProcedure rec name (Just name) args body span
    descendBiM perProgramUnit subprogs
    return p

perProgramUnit p = do
    resetTemps
    descendBiM perBlock p
    return p

addProcedure :: Params
    => Bool -- Recursive or not
    -> F.Name
    -> Maybe F.Name -- Maybe return name
    -> (Maybe (F.AList F.Expression A1)) -- Arguments
    -> [F.Block A1] -- Body
    -> FU.SrcSpan
    -> State UnitEnv ()
addProcedure rec name rname args body span = do
    -- Do just the declarations first
    let ?argumentDecls = True in mapM_ perStatement [s | s@(F.StDeclaration {}) <- universeBi body :: [F.Statement A1]]
    --descendBiM perBlock body
    uenv <- gets varColEnv
    resultVar <- case rname of
                   Just rname ->
                     case (lookupWithoutSrcSpanRealName rname uenv) of
                        Just (uvar, _) -> return $ Just uvar
                        Nothing        -> do m <- addCol Variable
                                             varColEnv << (VarBinder (rname, span), (VarCol m, []))
                                             return $ (Just (VarCol m))
                   Nothing -> return Nothing

    let argVars = fromMaybe [] (fmap (map (lookupUnitByName uenv) . F.aStrip) args)

    procedureEnv << (name, (resultVar, argVars))
    descendBiM perBlock body
    if rec
      then do descendBiM perBlock body
              return ()
      else return ()
    -- Intermediate solve for procedures (subroutines & functions)
    ifDebug (report <<++ "Pre doing row reduce")
    consistent <- solveSystemM ""
    success =: consistent

    linearSystem =. reduceRows 1
    ifDebug (report <<++ "Post doing reduce")
    ifDebug (debugGaussian)
    return ()
  where
    lookupUnitByName uenv ve@(F.ExpValue _ _ (F.ValVariable _)) =
        maybe (VarCol 1) fst $ lookupWithoutSrcSpan v uenv
          where v = FA.varName ve


-- Check units per block
perBlock :: Params
         => F.Block A1
         -> State UnitEnv (F.Block A1)
perBlock b@(F.BlComment ann span _) = do
    case (unitSpec (FA.prevAnnotation ann), unitBlock (FA.prevAnnotation ann)) of
      -- Found a unit comment associated to a block
      (Just (Parser.UnitAssignment (Just vars) unitsAST), Just block) -> do
         let units = toUnitInfo unitsAST
         unitsConverted <- convertUnit units
         case block of
              bl@(F.BlStatement ann span _ (F.StDeclaration _ _ _ _ decls)) ->
                flip mapM_ vars (\var ->
                  mapM_ (processVar' (Just var) [unitsConverted]) (getNamesAndInits decls))
              _ -> return ()
      -- Found a derived unit declaration
      (Just (Parser.UnitAlias name unitsAST), _) -> do
         let unitInfo = toUnitInfo unitsAST
         learnDerivedUnit (name, unitInfo)
      _ -> return ()
    return b

  where
    processVar' varReal unitC (var, init, span) = do
       if (Just (realName var) == varReal) then hasDeclaration <<++ var else return ()
       processVar varReal unitC (var, init, span)
    learnDerivedUnit (name, spec) =
          do denv <- gets derivedUnitEnv
             when (isJust $ lookup name denv) $ error "Redeclared unit of measure"
             unit <- convertUnit spec
             denv <- gets derivedUnitEnv
             when (isJust $ lookup name denv) $ error "Recursive unit-of-measure definition"
             derivedUnitEnv << (name, unit)
    -- Note we get the real names here since we are working with a user-specified
    -- variable which is associated to this decl
    getNamesAndInits x =
        [(FA.varName e, i, s) | (F.DeclVariable _ _ e@(F.ExpValue _ s (F.ValVariable v)) _ i) <-
                    (universeBi (F.aStrip x) :: [F.Declarator A1])]
     ++ [(FA.varName e, i, s) | (F.DeclArray _ _ e@(F.ExpValue _ s (F.ValVariable v)) _ _ i) <-
                    (universeBi (F.aStrip x) :: [F.Declarator A1])]
     -- TODO: generate constraints for indices
    dimDeclarators x = concat
         [F.aStrip dims | (F.DeclArray _ _ _ dims _ _) <-
                    (universeBi (F.aStrip x) :: [F.Declarator A1])]

{- TODO: investigate
    unitVarCat :: Variable -> Maybe ProcedureNames -> UnitVarCategory
    unitVarCat v proc | Just (n, r, args) <- proc, v `elem` args = Argument
                  | otherwise                                = Variable
-}

perBlock b@(F.BlStatement _ _ _ s) = do
    perStatement s
    return b

perBlock b = do
    mapM_ perDoSpecification (universeBi b)
    mapM_ perExpr (universeBi b)
    descendBiM (plumb perBlock) b
    return b

processVar :: Params
           => Maybe F.Name
           -> [UnitConstant]
           -> (F.Name, Maybe (F.Expression A1), FU.SrcSpan)
           -> State UnitEnv ()
processVar (Just dvar) units (v, initExpr, span) | dvar == (realName v) = do
      system <- gets linearSystem
      let m = ncols (fst system) + 1
      unitVarCats <<++ (if ?argumentDecls then Argument else Variable) -- TODO: check how much we need this: (unitVarCat v proc)
      extendConstraints units
      varColEnv << (VarBinder (v, span), (VarCol m, []))
      uv <- gets varColEnv
      -- If the declaration has a null expression, do not create a unifying variable
      case initExpr of
          Nothing -> return ()
          Just e  -> do
            uv <- perExpr e
            mustEqual False (VarCol m) uv
            return ()
processVar dvar units (v, initExpr, span) | otherwise = return ()


-- Do specifications (e.g. i = 1, n, s) enforces an equality constraint on the
-- units between each component (all must have the same unit)
perDoSpecification ::
     Params
  => F.DoSpecification A1 -> State UnitEnv ()
perDoSpecification (F.DoSpecification _ _
                      st@(F.StExpressionAssign _ _ ei e0) en step) = do
   uiv <- perExpr ei
   e0v <- perExpr e0
   env <- perExpr en
   mustEqual True uiv e0v
   mustEqual True e0v env
   case step of
     Nothing    -> return ()
     Just stepE -> do stepv <- perExpr stepE
                      mustEqual True env stepv
                      return ()

-- TODO: see if we need to insert anymore statement-specific constraints here
perStatement ::
     Params
   => F.Statement A1 -> State UnitEnv ()
perStatement (F.StDeclaration _ span spec atr decls) = do
    uenv <- gets varColEnv
    mapM_ (\(v, i, s) -> if notAlreadyDeclared uenv v
                           then processVar (Just (realName v)) [] (v, i, s)
                           else return ())  (getNamesAndInits decls)
  where
    -- Variable may have been declared already due to link with comment
    notAlreadyDeclared uenv v =
      case lookupWithoutSrcSpan v uenv of
        Nothing -> True
        Just _  -> False

    getNamesAndInits x =
        [(FA.varName e, i, s) |
           (F.DeclVariable _ _ e@(F.ExpValue _ s (F.ValVariable _)) _ i)
              <- (universeBi (F.aStrip x) :: [F.Declarator A1])]
     ++ [(FA.varName e, i, s) |
           (F.DeclArray _ _ e@(F.ExpValue _ s (F.ValVariable _)) _ _ i)
              <- (universeBi (F.aStrip x) :: [F.Declarator A1])]

perStatement (F.StExpressionAssign _ span e1 e2) = do
    uv1 <- perExpr e1
    uv2 <- perExpr e2
    mustEqual False uv1 uv2
    return ()

perStatement (F.StPointerAssign _ _ e1 e2) = do
    uv1 <- perExpr e1
    uv2 <- perExpr e2
    mustEqual False uv1 uv2
    return ()

perStatement (F.StCall _ _ e@(F.ExpValue _ _ (F.ValVariable vReal)) args) = do
    uvs <- fromMaybe (return []) (fmap (mapM perArgument . F.aStrip) args)
    case (lookup (map toUpper vReal) intrinsicsDict) of
       Just fun -> fun vReal
       Nothing  -> return ()
    uv@(VarCol uvn) <- anyUnits Temporary
    --uvs <- inferArgUnits
    --let uvs' = justArgUnits args uvs
    calls << (vReal, (Just uv, uvs))


perStatement s = do
    mapM_ perDoSpecification (universeBi s)
    descendBiM (plumb perExpr) s
    return ()


inferInterproceduralUnits ::
    Params => F.ProgramFile A1 -> State UnitEnv ()
inferInterproceduralUnits x =
  do --reorderColumns
     if ?criticals then reorderVarCols else return ()
     consistent <- solveSystemM "inconsistent"
     if consistent then
         do system <- gets linearSystem
            let dontAssumeLiterals = case ?assumeLiterals of
                                       Poly     -> True
                                       Unitless -> False
                                       Mixed    -> False
            inferInterproceduralUnits' x dontAssumeLiterals system -- edited
            return ()
     else
         return ()

inferInterproceduralUnits' ::
    Params => F.ProgramFile A1 -> Bool -> LinearSystem
           -> State UnitEnv (F.ProgramFile A1)
inferInterproceduralUnits' x haveAssumedLiterals system1 =
  do addInterproceduralConstraints x
     consistent <- solveSystemM "inconsistent"
     if not consistent then
          do  linearSystem =: system1
              return x
      else do
        system2 <- gets linearSystem
        if system1 == system2
          then if ?criticals then nextStep else checkUnderdeterminedM >> nextStep
          else inferInterproceduralUnits' x haveAssumedLiterals system2
  where nextStep | haveAssumedLiterals = return x
                 | otherwise           = do consistent <- assumeLiteralUnits
                                            if not consistent
                                             then return x
                                             else do system3 <- gets linearSystem
                                                     inferInterproceduralUnits' x True system3

assumeLiteralUnits :: (?solver :: Solver, ?debug :: Bool) => State UnitEnv Bool
assumeLiteralUnits =
  do system@(matrix, vector) <- gets linearSystem
     mapM_ assumeLiteralUnits' [1 .. ncols matrix]
     consistent <- solveSystemM "underdetermined"
     when (not consistent) $ linearSystem =: system
     return consistent

assumeLiteralUnits' m =
      do (matrix, vector) <- gets linearSystem
         ucats <- gets unitVarCats
         let n = find (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
             m' = n >>= (\n -> find (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix])
             nonLiteral n m = matrix ! (n, m) /= 0 && ucats !! (m - 1) /= (Literal True)
             m's = n >>= (\n -> find (nonLiteral n) [1 .. ncols matrix])
         when (ucats !! (m - 1) == (Literal True) && (m' /= Just m || isJust m's)) $ do
           n' <- addRow
           modify $ liftUnitEnv $ setElem 1 (n', m)

addInterproceduralConstraints ::
    (?debug :: Bool) => F.ProgramFile A1 -> State UnitEnv ()
addInterproceduralConstraints x =
  do
    cs <- gets calls
    penv <- gets procedureEnv
    mapM_ (addCall penv) cs
  where
    addCall penv (name, (result, args)) =
      do case lookup name penv of
           Just (r, as) -> let (r1, r2) = decodeResult result r
                           in handleArgs (args ++ r1) (as ++ r2)
           Nothing      -> return ()

    handleArgs actualVars dummyVars =
      do order <- gets reorderedCols
         let actual = map (\(VarCol uv) -> uv) actualVars
             dummy = map (\(VarCol uv) -> uv) dummyVars
         mapM_ (handleArg $ zip dummy actual) dummy

    -- experimentation but now deprecated.
{-
    handleArgNew dummyToActual dummy =
        do grid0 <- debugGaussian'
           mapM (\(l, r) -> do n <- addRow
                               modify $ liftUnitEnv $ setElem 1 (n, l)
                               modify $ liftUnitEnv $ setElem (-1) (n, r)
                ) dummyToActual
           grid1 <- debugGaussian'
           if (grid0 == grid1) then
               return ()
           else
             do report <<++ "HANDLED AND DIFFERENT!"
                report <<++ ("\n" ++ grid0)
                report <<++ ("\n" ++ grid1)
                return ()-}

    -- TODO: this can be optimised
    handleArg dummyToActual dummy =
      do (matrix, vector) <- gets linearSystem
         --grid0 <- debugGaussian'
         ifDebug (debugGaussian)

         ifDebug (report <<++ ("hArg - " ++ show dummyToActual ++ "-" ++ show dummy))

         let -- find the first row with a non-zero column for the variable
             n = maybe 1 id $ find (\n -> matrix ! (n, dummy) /= 0) [1 .. nrows matrix]

             -- find the first non-zero column on the row just selected
             Just m = find (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]

         ifDebug (report <<++ ("n = " ++ show n ++ ", m = " ++ show m))

         if (m == dummy) then
           do  let -- Get list of columns with non-zero coefficients to the right of the focus
                   ms = filter (\m -> matrix ! (n, m) /= 0) [m .. ncols matrix]

                   -- Get the list of columns to which the non-zero coeffecients
                   -- are paired by 'dummyToActual' relation.
                   m's = mapMaybe (flip lookup dummyToActual) ms
                   pairs = --if (length m's == 1) then -- i.e. there is not a
                           --  --direct relationship between variable and return
                           --    zip ms (repeat (head m's))
                           --else
                               (zip ms m's)

               ifDebug (report <<++ ("ms = " ++ show ms ++ ", m's' = "
                               ++ show m's
                               ++ ", their zip = " ++ show pairs
                               ++ " dA = " ++ show dummyToActual))

               if (True) -- length m's == length ms)
                 then do { newRow <- addRow' $ vector !! (n - 1);
--                           mapM_ (handleArgPair matrix n newRow) pairs ; }
                           mapM_ (handleArgPair matrix n newRow) dummyToActual ; }
                 else return ()
         else
             return ()

    -- Copy the row
    handleArgPair matrix n newRow (m, m') = do
        modify $ liftUnitEnv $ setElem (matrix ! (n, m)) (newRow, m')

    decodeResult (Just r1) (Just r2) = ([r1], [r2])
    decodeResult Nothing Nothing = ([], [])
    decodeResult (Just _) Nothing = error "Subroutine used as a function!"
    decodeResult Nothing (Just _) = error "Function used as a subroutine!"

data BinOpKind = AddOp | MulOp | DivOp | PowerOp | LogicOp | RelOp
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

(<**>) :: Maybe a -> Maybe a -> Maybe a
Nothing <**> x = x
(Just x) <**> y = (Just x)


{- OLD
    uenv <- gets varColEnv
    case lookupWithoutSrcSpan v uenv of
    let (VarName _ v, args) = head names

    case lookupWithoutSrcSpan v uenv of
       -- array variable?
       Just (uv, uvs@(_:_)) -> inferArgUnits' uvs >> return uv
       -- function call?
       Nothing | not (null args) -> do case (lookup (map toUpper v) intrinsicsDict) of
                                          Just fun -> fun v
                                          Nothing  -> return () -- error $ "I don't know the intrinsic " ++ v -- return ()
                                       uv@(VarCol uvn) <- anyUnits Temporary
                                       debugInfo << (uvn, (srcSpan ve, pprint ve))
                                       uvs <- inferArgUnits
                                       let uvs' = justArgUnits args uvs
                                       calls << (v, (Just uv, uvs'))
                                       return uv
       -- scalar variable or external function call?
       Just (uv, []) -> inferArgUnits >> return uv
       -- default specifier
       _ | v == "*" -> inferLiteral ve
       -- just bad code
       x -> case lookupCaseInsensitive v penv of
              Just (Just uv, argUnits) ->
                   if (null args) then inferArgUnits' argUnits >> return uv
                   else  do uv <- anyUnits Tempgit orary
                            uvs <- inferArgUnits
                            let uvs' = justArgUnits args uvs
                            calls << (v, (Just uv, uvs'))
                            return uv

              Nothing -> error $ "\n" ++ (showSrcFile . srcSpan $ ve) ++ ": undefined variable " ++ v ++ " at " ++ (showSrcSpan . srcSpan $ ve)
  where inferArgUnits = sequence [mapM perExpr exprs | (_, exprs) <- names]
        inferArgUnits' uvs = sequence [(perExpr expr) >>= (\uv' -> mustEqual True uv' uv) | ((_, exprs), uv) <- zip names uvs, expr <- exprs, not (nullF.Expression [expr])]

        justArgUnits [NullF.Expression _ _] _ = []  -- zero-argument function call
        justArgUnits _ uvs = head uvs
        -}

-- TODO, create unit vars for every index and make sure consistent
perIndex :: Params => F.Name -> F.Index A1 -> State UnitEnv ()
perIndex v (F.IxSingle _ _ _ e) = return ()
 {-
  uenv <- gets varColEnv
  arrV <- case lookpuWithoutSrcSpan v env
           Nothing -> do
              uv@(ValCol uvn) <- anyUnits Temporary
              return uv
           Just (uv, uvs) ->
-}

perExpr :: Params => F.Expression A1 -> State UnitEnv VarCol
perExpr e@(F.ExpValue _ span (F.ValVariable _)) = do
    let v = FA.varName e
    uenv <- gets varColEnv
    case lookupWithoutSrcSpan v uenv of
      Nothing ->
        case lookupWithoutSrcSpanRealName (realName v) uenv of
           Nothing -> do uv@(VarCol uvn) <- anyUnits Temporary
                         return uv
           Just (uv, _) -> return uv
      Just (uv, _) -> return uv

perExpr e@(F.ExpValue _ span v) = perLiteral v
  where
    perLiteral :: Params => F.Value A1 -> State UnitEnv VarCol
    perLiteral val = do
      uv@(VarCol uvn) <- anyUnits (Literal (?assumeLiterals /= Mixed))
      debugInfo << (uvn, (span, show val))
      return uv

perExpr e@(F.ExpBinary _ _ op e1 e2) = do
    uv1 <- perExpr e1
    uv2 <- perExpr e2
    (VarCol n) <- case binOpKind op of
                    AddOp   -> mustEqual True uv1 uv2
                    MulOp   -> mustAddUp uv1 uv2 1 1
                    DivOp   -> mustAddUp uv1 uv2 1 (-1)
                    PowerOp -> powerUnits uv1 e2
                    LogicOp -> mustEqual True uv1 uv2
                    RelOp   -> do mustEqual True uv1 uv2
                                  return $ VarCol 1
    debugInfo << (n, (FU.getSpan e, pprint e))
    return (VarCol n)
  where
    pprint e = "" -- TODO pprint

perExpr (F.ExpUnary _ _ _ e) = perExpr e
perExpr (F.ExpSubscript _ _ e alist) = do
    descendBiM (plumb (perIndex (FA.varName e))) alist
    perExpr e

perExpr f@(F.ExpFunctionCall _ span e@(F.ExpValue _ _ (F.ValVariable vReal)) args) = do
    uv <- anyUnits Temporary
    argsU <- fromMaybe (return []) (fmap (mapM perArgument . F.aStrip) args)
    calls << (vReal, (Just uv, argsU))
    return uv

perExpr f@(F.ExpDataRef _ _ e1 e2) = do
    perExpr e2
    perExpr e1
perExpr f@(F.ExpImpliedDo _ _ exprs spec) = do
    perDoSpecification spec
    uv <- anyUnits Temporary
    exprsU <- mapM perExpr (F.aStrip exprs)
    mapM_ (mustEqual True uv) exprsU
    return uv
perExpr f@(F.ExpInitialisation _ _ exprs) = do
    uv <- anyUnits Temporary
    exprsU <- mapM perExpr (F.aStrip exprs)
    mapM_ (mustEqual True uv) exprsU
    return uv
perExpr f@(F.ExpReturnSpec _ _ e) = do
    perExpr e

perArgument :: Params =>
    F.Argument A1 -> State UnitEnv VarCol
perArgument (F.Argument _ _ _ expr) = perExpr expr

handleExpression :: Params
    => F.Expression A1
    -> State UnitEnv (F.Expression A1)
handleExpression x = do
    perExpr x
    return x

-- TODO: error handling in powerUnits
powerUnits :: Params
           => VarCol -> F.Expression A1 -> State UnitEnv VarCol

powerUnits (VarCol uv) (F.ExpValue _ _ (F.ValInteger powerString)) =
  case fmap (fromInteger . fst) $ listToMaybe $ reads powerString of
    Just power -> do
      m <- addCol Temporary
      n <- addRow
      modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem power (n, uv)
      return $ VarCol m
    Nothing -> mustEqual False (VarCol uv) (VarCol 1)
powerUnits uv e =
  do mustEqual False uv (VarCol 1)
     uv <- perExpr e
     mustEqual False uv (VarCol 1)

lookupWithoutSrcSpanRealName :: Params => F.Name -> [(VarBinder, a)] -> Maybe a
lookupWithoutSrcSpanRealName v env = snd `fmap` find f env
      where
        f (VarBinder (w, _), _) = (map toUpper (realName w)) == map toUpper v
