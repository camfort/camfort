{- |
Module      :  Camfort.Specification.Units.Analysis.Infer
Description :  Analysis for inferring units.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Camfort.Specification.Units.Analysis.Infer
  ( InferenceReport
  , getInferred
  , inferUnits
  ) where

import           Data.Data (Data)
import           Data.Generics.Uniplate.Operations
  (universeBi)
import           Data.List (sort)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe, maybeToList)
import           GHC.Generics (Generic)

import qualified Language.Fortran.AST           as F
import qualified Language.Fortran.Analysis      as FA
import qualified Language.Fortran.Util.Position as FU

import Camfort.Analysis (writeDebug)
import Camfort.Analysis.Annotations (Annotation)
import Camfort.Specification.Units.Analysis
  (UnitsAnalysis, puName, puSrcName, runInference)
import Camfort.Specification.Units.Analysis.Consistent
  (ConsistencyError, ConsistencyReport(..), checkUnits)
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.InferenceBackendSBV (inferVariables)
import Camfort.Specification.Units.InferenceBackend (chooseImplicitNames)
import Camfort.Specification.Units.Monad

data ExpInfo = ExpInfo
  { eiSrcSpan :: FU.SrcSpan
  , eiVName   :: F.Name
  , eiSName   :: F.Name
  } deriving (Show, Eq, Ord, Typeable, Data, Generic)

-- | Report from unit inference.
data InferenceReport =
  Inferred (F.ProgramFile UA) [(VV, UnitInfo)]

instance Show InferenceReport where
  show (Inferred pf vars) =
    concat ["\n", fname, ":\n", unlines [ expReport ei | ei <- expInfo ]]
    where
      expReport (ei, u) = "  " ++ showSrcSpan (eiSrcSpan ei) ++ " unit " ++ show u ++ " :: " ++ eiSName ei
      showSrcSpan :: FU.SrcSpan -> String
      showSrcSpan (FU.SrcSpan l _) = show l
      fname = F.pfGetFilename pf
      expInfo = [ (ei, u) | ei <- declVariableNames
                          , u <- maybeToList ((eiVName ei, eiSName ei) `lookup` vars) ]
      -- | List of declared variables (including both decl statements & function returns, defaulting to first)
      declVariableNames :: [ExpInfo]
      declVariableNames = sort . M.elems $ M.unionWith (curry fst) declInfo puInfo
        where
          declInfo = M.fromList [ (eiVName ei, ei) | ei <- declVariableNamesDecl ]
          puInfo   = M.fromList [ (eiVName ei, ei) | ei <- declVariableNamesPU ]
      declVariableNamesDecl :: [ExpInfo]
      declVariableNamesDecl = flip mapMaybe (universeBi pf :: [F.Declarator UA]) $ \ d -> case d of
        F.DeclVariable _ ss v@(F.ExpValue _ _ (F.ValVariable _)) _ _   -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
        F.DeclArray    _ ss v@(F.ExpValue _ _ (F.ValVariable _)) _ _ _ -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
        _                                                             -> Nothing
      declVariableNamesPU :: [ExpInfo]
      declVariableNamesPU = flip mapMaybe (universeBi pf :: [F.ProgramUnit UA]) $ \ pu -> case pu of
        F.PUFunction _ ss _ _ _ _ (Just v@(F.ExpValue _ _ (F.ValVariable _))) _ _ -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
        F.PUFunction _ ss _ _ _ _ Nothing _ _                                     -> Just (ExpInfo ss (puName pu) (puSrcName pu))
        _                                                                         -> Nothing

getInferred :: InferenceReport -> [(VV, UnitInfo)]
getInferred (Inferred _ vars) = vars

-- | Check and infer units-of-measure for a program
--
-- This produces an output of all the unit information for a program.
inferUnits :: UnitsAnalysis (F.ProgramFile Annotation) (Either ConsistencyError InferenceReport)
inferUnits = do
  (eVars, state, logs) <- runInference (chooseImplicitNames <$> runInferVariables)
  consistency <- checkUnits
  writeDebug logs
  let pfUA = usProgramFile state -- the program file after units analysis is done
  pure $ case consistency of
           Consistent{}     -> Right $ Inferred pfUA eVars
           Inconsistent err -> Left err

-- | Return a list of variable names mapped to their corresponding
-- unit that was inferred.
runInferVariables :: UnitSolver [(VV, UnitInfo)]
runInferVariables = inferVariables <$> getConstraints
