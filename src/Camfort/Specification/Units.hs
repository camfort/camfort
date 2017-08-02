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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Camfort.Specification.Units
  ( inferUnits
  , synthesiseUnits
  , chooseImplicitNames
  ) where

import qualified Data.Map.Strict as M
import Data.Data
import Data.List (sort, nub, inits)
import Data.Maybe (fromMaybe, maybeToList, mapMaybe)
import Data.Generics.Uniplate.Operations
import GHC.Generics (Generic)

import Camfort.Analysis
  (analysisInput, analysisModFiles, analysisParams, writeDebug)
import Camfort.Analysis.Annotations

-- Provides the types and data accessors used in this module
import           Camfort.Specification.Units.Analysis
  (UnitsAnalysis, puName, puSrcName, runInference)
import           Camfort.Specification.Units.Analysis.Consistent
  (ConsistencyError, ConsistencyReport(Consistent, Inconsistent), checkUnits)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.InferenceFrontend (runInferVariables)
import           Camfort.Specification.Units.Monad
import           Camfort.Specification.Units.Synthesis (runSynthesis)

import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU

-- *************************************
--   Unit inference (top - level)
--
-- *************************************

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

-- | Report from unit inference.
data InferenceReport =
  Inferred (F.ProgramFile UA) [(VV, UnitInfo)]

instance Show InferenceReport where
  show (Inferred pf vars) =
    concat ["\n", fname, ":\n", unlines [ expReport ei | ei <- expInfo ]]
    where
      expReport (ExpInfo ss vname sname, u) = "  " ++ showSrcSpan ss ++ " unit " ++ show u ++ " :: " ++ sname
      fname = F.pfGetFilename pf
      expInfo = [ (ei, u) | ei@(ExpInfo _ vname sname) <- declVariableNames pf
                          , u <- maybeToList ((vname, sname) `lookup` vars) ]
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


getInferred :: InferenceReport -> [(VV, UnitInfo)]
getInferred (Inferred _ vars) = vars

{-| Check and infer units-of-measure for a program
    This produces an output of all the unit information for a program -}
inferUnits :: UnitsAnalysis (F.ProgramFile Annotation) (Either ConsistencyError InferenceReport)
inferUnits = do
  (eVars, state, logs) <- runInference (chooseImplicitNames <$> runInferVariables)
  consistency <- checkUnits
  writeDebug logs
  let pfUA = usProgramFile state -- the program file after units analysis is done
  pure $ case consistency of
           Consistent{}     -> Right $ Inferred pfUA eVars
           Inconsistent err -> Left err

synthesiseUnits :: Char
                -> UnitsAnalysis
                   (F.ProgramFile Annotation)
                   (Either ConsistencyError (InferenceReport, F.ProgramFile Annotation))
{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits marker = do
  infRes <- inferUnits
  case infRes of
    Left err       -> pure $ Left err
    Right inferred -> do
      (_, state, logs) <- runInference
        (runSynthesis marker . chooseImplicitNames . getInferred $ inferred)
      let pfUA    = usProgramFile state -- the program file after units analysis is done
          pfFinal = fmap (UA.prevAnnotation . FA.prevAnnotation) pfUA -- strip annotations
      pure . Right $ (inferred, pfFinal)

--------------------------------------------------

showSrcSpan :: FU.SrcSpan -> String
showSrcSpan (FU.SrcSpan l u) = show l

data ExpInfo = ExpInfo { expInfoSrcSpan :: FU.SrcSpan, expInfoVName :: F.Name, expInfoSName :: F.Name }
  deriving (Show, Eq, Ord, Typeable, Data, Generic)
