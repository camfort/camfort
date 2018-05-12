{-# LANGUAGE OverloadedStrings #-}

module Camfort.Analysis.ImplicitNoneSpec (spec) where

import Data.Binary (encodeFile)
import Data.List (sort)
import System.Directory (createDirectory)
import System.FilePath ((</>), (<.>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Lens

import Language.Fortran.Util.ModFile
import Language.Fortran.AST (ProgramUnitName(..))

import Camfort.Analysis.ModFile (getModFiles)
import Camfort.Analysis.Simple
import Camfort.Analysis.TestUtils
import Camfort.Analysis hiding (describe)

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

spec :: Test.Spec
spec =
  describe "implicitNone" $ do
    it "basic" $ do
      testSingleFileAnalysis (testInputSources $ fixturesDir </> "implicitnone1.f90") (generalizePureAnalysis . checkImplicitNone) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "s"]
    it "misplaced statements" $ do
      testSingleFileAnalysis (testInputSources $ fixturesDir </> "implicitnone2.f90") (generalizePureAnalysis . checkImplicitNone) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "implicitnone1", Named "implicitnone2"]
    it "interfaces are separate scope" $ do
      testSingleFileAnalysis (testInputSources $ fixturesDir </> "implicitnone3.f90") (generalizePureAnalysis . checkImplicitNone) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "s"]

fixturesDir :: FilePath
fixturesDir = "tests" </> "fixtures"
