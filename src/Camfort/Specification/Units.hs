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

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Camfort.Specification.Units
  (checkUnits, inferUnits, compileUnits, synthesiseUnits, inferCriticalVariables, chooseImplicitNames)
where

import qualified Data.Map.Strict as M
import Data.Data
import Data.List (intercalate, find, sort, group, nub, inits)
import Data.Maybe (fromMaybe, maybeToList, mapMaybe, maybe)
import Data.Binary
import Data.Generics.Uniplate.Operations
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics (Generic)

import Camfort.Helpers
import Camfort.Analysis.Annotations
import Camfort.Input
import Camfort.Reprint (subtext)

-- Provides the types and data accessors used in this module
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Monad
import Camfort.Specification.Units.InferenceBackend
import qualified Camfort.Specification.Units.InferenceBackendZ3 as Z
import Camfort.Specification.Units.InferenceFrontend
import Camfort.Specification.Units.Synthesis (runSynthesis)

import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.Util.ModFile


-- *************************************
--   Unit inference (top - level)
--
-- *************************************

inferCriticalVariables
  :: UnitOpts -> (Filename, SourceText, F.ProgramFile Annotation) -> (Report, Int)

{-| Infer one possible set of critical variables for a program -}
inferCriticalVariables uo (fname, _, pf)
  | Right vars <- eVars = okReport vars
  | Left exc   <- eVars = (errReport exc, -1)
  where
    -- Format report
    okReport []   = (logs ++ "\n" ++ fname
                         ++ ":No additional annotations are necessary.\n", 0)
    okReport vars = ( logs ++ "\n" ++ fname ++ ": "
                           ++ show numVars
                           ++ " variable declarations suggested to be given a specification:\n"
                           ++ unlines [ "    " ++ declReport d | d <- M.toList dmapSlice ]
                    , numVars)
      where
        varNames  = map unitVarName vars
        dmapSlice = M.filterWithKey (\ k _ -> k `elem` varNames) dmap
        numVars   = M.size dmapSlice

    declReport (v, (dc, ss)) = vfilename ++ " (" ++ showSrcSpan ss ++ ")    " ++ fromMaybe v (M.lookup v uniqnameMap)
      where vfilename = fromMaybe fname $ M.lookup v fromWhereMap

    unitVarName (UnitVar (v, _))                 = v
    unitVarName (UnitParamVarUse (_, (v, _), _)) = v
    unitVarName _                                = "<bad>"

    errReport exc = logs ++ "\n" ++ fname ++ ":\n" ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = FAR.extractNameMap pfRenamed }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ do
      modifyTemplateMap . const . cuTemplateMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      modifyNameParamMap . const . cuNameParamMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      initInference
      runCriticalVariables
    pfUA = usProgramFile state -- the program file after units analysis is done

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf

    -- Map of all declarations
    dmap = extractDeclMap pfRenamed `M.union` combinedDeclMap (M.elems (uoModFiles uo))

    mmap' = extractModuleMap pfRenamed `M.union` mmap -- post-parsing
    -- unique name -> src name across modules
    uniqnameMap = M.fromList [
                (FA.varName e, FA.srcName e) |
                e@(F.ExpValue _ _ (F.ValVariable {})) <- universeBi pfRenamed :: [F.Expression UA]
                -- going to ignore intrinsics here
              ] `M.union` (M.unions . map (M.fromList . map (\ (a, (b, _)) -> (b, a)) . M.toList) $ M.elems mmap')
    fromWhereMap = genUniqNameToFilenameMap . M.elems $ uoModFiles uo

checkUnits :: UnitOpts -> (Filename, B.ByteString, F.ProgramFile Annotation) -> Report
{-| Check units-of-measure for a program -}
checkUnits uo (fname, fileText, pf)
  | Right mCons <- eCons = okReport mCons
  | Left exc    <- eCons = errReport exc
  where
    -- Format report
    okReport Nothing     = logs ++ "\n" ++ fname ++ ": Consistent. " ++ show nVars ++ " variables checked."
    okReport (Just cons) = logs ++ "\n" ++ fname ++ ": Inconsistent:\n" ++ reportErrors cons ++ "\n\n" ++
                           unlines (map show constraints)

    reportErrors cons = unlines [ maybe "" showSS ss ++ str | (ss, str) <- reports ]
      where
        reports = map head . group . sort . map reportError . filter relevantConstraints $ cons
        showSS  = (++ ": ") . (" - at "++) . showSrcSpan

        relevantConstraints c = not (isPolymorphic0 c) && not (isReflexive c)

        isPolymorphic0 (ConEq (UnitParamLitAbs {}) _) = True
        isPolymorphic0 (ConEq _ (UnitParamLitAbs {})) = True
        isPolymorphic0 _                         = False

        isReflexive (ConEq u1 u2) = u1 == u2

    reportError con = (span, pprintConstr srcText (orient (unrename nameMap con)) ++ additionalInfo con)
      where
        srcText = findCon con >>= fst
        span = findCon con >>= (return . snd)
        -- Create additional info for errors
        additionalInfo con =
           if null (errorInfo con)
           then ""
           else "\n    instead" ++ intercalate "\n" (mapNotFirst (pad 10) (errorInfo con))
        -- Create additional info about inconsistencies involving variables
        errorInfo con =
            [" '" ++ sName ++ "' is '" ++ pprintUnitInfo Nothing (unrename nameMap u) ++ "'"
              | UnitVar (vName, sName) <- universeBi con
              , u                       <- findUnitConstrFor con vName ]
        -- Find unit information for variable constraints
        findUnitConstrFor con v = mapMaybe (\con' -> if con == con'
                                                     then Nothing
                                                     else constrainedTo v con')
                                           (concat $ M.elems templateMap)
        constrainedTo v (ConEq (UnitVar (v', _)) u) | v == v' = Just u
        constrainedTo v (ConEq u (UnitVar (v', _))) | v == v' = Just u
        constrainedTo _ _ = Nothing

        mapNotFirst f [] = []
        mapNotFirst f (x : xs) =  x : (map f xs)

        orient (ConEq u (UnitVar v)) = ConEq (UnitVar v) u
        orient (ConEq u (UnitParamVarUse v)) = ConEq (UnitParamVarUse v) u
        orient c = c

        pad o = (++) (replicate o ' ')

        srcSpan con | Just (_, ss) <- findCon con = showSrcSpan ss ++ " "
                    | otherwise                   = ""

    -- Find a given constraint within the annotated AST. FIXME: optimise

    findCon :: Constraint -> Maybe (Maybe SourceText, FU.SrcSpan)
    findCon con = lookupWith (eq con) constraints
      where eq c1 c2 = or [ conParamEq c1 c2' | c2' <- universeBi c2 ]


    constraints = [ (c, (Just (getSnippet srcSpan), srcSpan))
                  | x <- universeBi pfUA :: [F.Expression UA]
                  , let srcSpan = FU.getSpan x
                  , c <- maybeToList (getConstraint x)
                  ] ++

                  [ (c, (Just (getSnippet srcSpan), srcSpan))
                  | x <- universeBi pfUA :: [F.Statement UA]
                  , let srcSpan = FU.getSpan x
                  , c <- maybeToList (getConstraint x)
                  ] ++

                  [ (c, (Just (getSnippet srcSpan), srcSpan))
                  | x <- universeBi pfUA :: [F.Argument UA]
                  , let srcSpan = FU.getSpan x
                  , c <- maybeToList (getConstraint x)
                  ] ++

                  [ (c, (Just (getSnippet srcSpan), srcSpan))
                  | x <- universeBi pfUA :: [F.Declarator UA]
                  , let srcSpan = FU.getSpan x
                  , c <- maybeToList (getConstraint x)
                  ] ++

                  -- Why reverse? So that PUFunction and PUSubroutine appear
                  -- first in the list, before PUModule.
                  reverse [ (c, (Nothing, srcSpan))
                  | x <- universeBi pfUA :: [F.ProgramUnit UA]
                  , let srcSpan = FU.getSpan x
                  , c <- maybeToList (getConstraint x)
                  ]

    getSnippet srcSpan = fst $ subtext (1,1) lower upper fileText
      where
        (FU.SrcSpan (FU.Position _ colL lnL) (FU.Position _ colU lnU)) = srcSpan
        lower = (lnL, colL)
        upper = (lnU, colU)

    varReport     = intercalate ", " . map showVar

    showVar (UnitVar (_, s)) = s
    showVar (UnitLiteral _)   = "<literal>" -- FIXME
    showVar _                 = "<bad>"

    errReport exc = logs ++ "\n" ++ fname ++ ":  " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eCons, state, logs) = runUnitSolver uOpts pfRenamed $ do
      modifyTemplateMap . const . cuTemplateMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      modifyNameParamMap . const . cuNameParamMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      initInference
      runInconsistentConstraints
    templateMap = usTemplateMap state
    pfUA :: F.ProgramFile UA
    pfUA = usProgramFile state -- the program file after units analysis is done

    -- number of 'real' variables checked, e.g. not parametric
    nVars = M.size . M.filter (not . isParametricUnit) $ usVarUnitMap state
    isParametricUnit u = case u of UnitParamPosAbs {} -> True; UnitParamPosUse {} -> True
                                   UnitParamVarAbs {} -> True; UnitParamVarUse {} -> True
                                   _ -> False

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

lookupWith :: (a -> Bool) -> [(a,b)] -> Maybe b
lookupWith f = fmap snd . find (f . fst)


-- | Create unique names for all of the inferred implicit polymorphic
-- unit variables.
chooseImplicitNames :: [(VV, UnitInfo)] -> [(VV, UnitInfo)]
chooseImplicitNames vars = replaceImplicitNames (genImplicitNamesMap vars) vars

genImplicitNamesMap :: Data a => a -> M.Map UnitInfo UnitInfo
genImplicitNamesMap x = M.fromList [ (absU, UnitParamEAPAbs (newN, newN)) | (absU, newN) <- zip absUnits newNames ]
  where
    absUnits = nub [ u | u@(UnitParamPosAbs _)             <- universeBi x ]
    eapNames = nub $ [ n | u@(UnitParamEAPAbs (_, n))      <- universeBi x ] ++
                     [ n | u@(UnitParamEAPUse ((_, n), _)) <- universeBi x ]
    newNames = filter (`notElem` eapNames) . map ('\'':) $ nameGen
    nameGen  = concatMap sequence . tail . inits $ repeat ['a'..'z']

replaceImplicitNames :: Data a => M.Map UnitInfo UnitInfo -> a -> a
replaceImplicitNames implicitMap = transformBi replace
  where
    replace u@(UnitParamPosAbs _) = fromMaybe u $ M.lookup u implicitMap
    replace u                     = u

{-| Check and infer units-of-measure for a program
    This produces an output of all the unit information for a program -}
inferUnits :: UnitOpts -> (Filename, B.ByteString, F.ProgramFile Annotation) -> IO Report
inferUnits uo (fname, fileText, pf)
  | Right []   <- eVars = return $ checkUnits uo (fname, fileText, pf)
  | Right vars <- eVars = do
    let cons = usConstraints state
    (putStrLn . unlines . map show . fromMaybe [] . snd) =<< Z.inferVariables cons
    return $ okReport vars
  | Left exc   <- eVars = return $ errReport exc
  where
    -- Format report
    okReport vars = logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ expReport ei | ei <- expInfo ] ++ show vars
      where
        expInfo = [ (ei, u) | ei@(ExpInfo _ vname sname) <- declVariableNames pfUA
                            , u <- maybeToList ((vname, sname) `lookup` vars) ]

    expReport (ExpInfo ss vname sname, u) = "  " ++ showSrcSpan ss ++ " unit " ++ show u ++ " :: " ++ sname

    errReport exc = logs ++ "\n" ++ fname ++ ":  " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ do
      modifyTemplateMap . const . cuTemplateMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      modifyNameParamMap . const . cuNameParamMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      initInference
      chooseImplicitNames `fmap` runInferVariables

    pfUA = usProgramFile state -- the program file after units analysis is done

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf

    nameMap = FAR.extractNameMap pfRenamed

combinedCompiledUnits :: ModFiles -> CompiledUnits
combinedCompiledUnits mfs = CompiledUnits { cuTemplateMap = M.unions tmaps
                                          , cuNameParamMap = M.unions nmaps }
  where
    cus = map mfCompiledUnits mfs
    tmaps = map cuTemplateMap cus
    nmaps = map cuNameParamMap cus

-- | Name of the labeled data within a ModFile containing unit-specific info.
unitsCompiledDataLabel = "units-compiled-data"

mfCompiledUnits :: ModFile -> CompiledUnits
mfCompiledUnits mf = case lookupModFileData unitsCompiledDataLabel mf of
  Nothing -> emptyCompiledUnits
  Just bs -> case decodeOrFail (LB.fromStrict bs) of
    Left _ -> emptyCompiledUnits
    Right (_, _, cu) -> cu

genUnitsModFile :: F.ProgramFile UA -> CompiledUnits -> ModFile
genUnitsModFile pf cu = alterModFileData f unitsCompiledDataLabel $ genModFile pf
  where
    f _ = Just . LB.toStrict $ encode cu

compileUnits :: UnitOpts -> [FileProgram] -> (String, [(Filename, B.ByteString)])
compileUnits uo fileprogs = (concat reports, concat bins)
  where
    (reports, bins) = unzip [ (report, bin) | fileprog <- fileprogs
                                            , let (report, bin) = compileUnits' uo fileprog ]

compileUnits' :: UnitOpts -> FileProgram -> (String, [(Filename, B.ByteString)])
compileUnits' uo (fname, pf)
  | Right cu <- eCUnits = okReport cu
  | Left exc <- eCUnits = errReport exc
  where
    -- Format report
    okReport cu = ( logs ++ "\n" ++ fname ++ ":\n" ++ if uoDebug uo then debugInfo else []
                     -- FIXME, filename manipulation (needs to go in -I dir?)
                    , [(fname ++ modFileSuffix, encodeModFile (genUnitsModFile pfTyped cu))] )
      where
        debugInfo = unlines [ n ++ ":\n  " ++ intercalate "\n  " (map show cs) | (n, cs) <- M.toList (cuTemplateMap cu) ] ++
                    unlines ("nameParams:" : (map show . M.toList $ cuNameParamMap cu))


    errReport exc = ( logs ++ "\n" ++ fname ++ ":  " ++ show exc
                    , [] )
    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eCUnits, state, logs) = runUnitSolver uOpts pfTyped $ do
      modifyTemplateMap . const . cuTemplateMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      modifyNameParamMap . const . cuNameParamMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      initInference
      runCompileUnits

    pfUA = usProgramFile state -- the program file after units analysis is done

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    tenv = combinedTypeEnv (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    pfTyped = fst . FAT.analyseTypesWithEnv tenv $ pfRenamed

    nameMap = FAR.extractNameMap pfTyped

synthesiseUnits :: UnitOpts
                -> Char
                -> (Filename, B.ByteString, F.ProgramFile Annotation)
                -> (Report, (Filename, F.ProgramFile Annotation))
{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits uo marker (fname, fileText, pf)
  | Right []   <- eVars = (checkUnits uo (fname, fileText, pf), (fname, pf))
  | Right vars <- eVars = (okReport vars, (fname, pfFinal))
  | Left exc   <- eVars = (errReport exc, (fname, pfFinal))
  where
    -- Format report
    okReport vars = logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ expReport ei | ei <- expInfo ]
      where
        expInfo = [ (ei, u) | ei@(ExpInfo _ vname sname) <- declVariableNames pfUA
                            , u <- maybeToList ((vname, sname) `lookup` vars) ]

    expReport (ExpInfo ss vname sname, u) = "  " ++ showSrcSpan ss ++ " unit " ++ show u ++ " :: " ++ sname

    errReport exc = logs ++ "\n" ++ fname ++ ":  " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ do
      modifyTemplateMap . const . cuTemplateMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      modifyNameParamMap . const . cuNameParamMap . combinedCompiledUnits . M.elems $ uoModFiles uo
      initInference
      runInferVariables >>= (runSynthesis marker . chooseImplicitNames)

    pfUA = usProgramFile state -- the program file after units analysis is done
    pfFinal = fmap prevAnnotation . fmap FA.prevAnnotation $ pfUA -- strip annotations

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

--------------------------------------------------

unrename nameMap = transformBi $ \ x -> x `fromMaybe` M.lookup x nameMap

showSrcSpan :: FU.SrcSpan -> String
showSrcSpan (FU.SrcSpan l u) = show l

data ExpInfo = ExpInfo { expInfoSrcSpan :: FU.SrcSpan, expInfoVName :: F.Name, expInfoSName :: F.Name }
  deriving (Show, Eq, Ord, Typeable, Data, Generic)

-- | List of declared variables (including both decl statements & function returns, defaulting to first)
declVariableNames :: F.ProgramFile UA -> [ExpInfo]
declVariableNames pf = sort . M.elems $ M.unionWith (curry fst) declInfo puInfo
  where
    declInfo = M.fromList [ (expInfoVName ei, ei) | ei <- declVariableNamesDecl pf ]
    puInfo   = M.fromList [ (expInfoVName ei, ei) | ei <- declVariableNamesPU pf ]

declVariableNamesDecl :: F.ProgramFile UA -> [ExpInfo]
declVariableNamesDecl pf = flip mapMaybe (universeBi pf :: [F.Declarator UA]) $ \ d -> case d of
  F.DeclVariable _ ss v@(F.ExpValue _ _ (F.ValVariable _)) _ _   -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
  F.DeclArray    _ ss v@(F.ExpValue _ _ (F.ValVariable _)) _ _ _ -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
  _                                                             -> Nothing
declVariableNamesPU :: F.ProgramFile UA -> [ExpInfo]
declVariableNamesPU pf = flip mapMaybe (universeBi pf :: [F.ProgramUnit UA]) $ \ pu -> case pu of
  F.PUFunction _ ss _ _ _ _ (Just v@(F.ExpValue _ _ (F.ValVariable _))) _ _ -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
  F.PUFunction _ ss _ _ _ _ Nothing _ _                                     -> Just (ExpInfo ss (puName pu) (puSrcName pu))
  _                                                                         -> Nothing
