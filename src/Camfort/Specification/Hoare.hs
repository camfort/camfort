{-# LANGUAGE OverloadedStrings #-}

module Camfort.Specification.Hoare where

import           Control.Monad.Except
import           Data.List (intersperse)

import qualified Language.Fortran.Analysis                 as FA
import qualified Language.Fortran.Analysis.BBlocks         as FAB
import qualified Language.Fortran.Analysis.Renaming        as FAR
import qualified Language.Fortran.AST                      as F
import qualified Language.Fortran.Util.Position            as F

import           Camfort.Analysis
import           Camfort.Analysis.Annotations
import           Camfort.Analysis.ModFile
import           Camfort.Helpers
import           Camfort.Input
import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.CheckFrontend
import           Camfort.Specification.Hoare.CheckBackend
import           Camfort.Specification.Hoare.Parser

getBlocks = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis . fmap hoareAnn0

check :: F.ProgramFile Annotation -> HoareAnalysis [HoareCheckResult]
check = invariantChecking . getBlocks

newtype HoareCheckResults = HoareCheckResults [HoareCheckResult]

instance Describe HoareCheckResults where
  describeBuilder (HoareCheckResults rs) = mconcat . intersperse "\n" . map describeBuilder $ rs

testOn :: FilePath -> IO ()
testOn fp = do
  (mfs, pfsSources) <- loadModAndProgramFiles simpleCompiler () fp fp []
  describePerFileAnalysis "invariant checking" (fmap HoareCheckResults . check) logOutputStd LogDebug mfs pfsSources

testHoare = testOn "samples/invariants/invariants.f90"
