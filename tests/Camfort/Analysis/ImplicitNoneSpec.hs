{-# LANGUAGE OverloadedStrings #-}

module Camfort.Analysis.ImplicitNoneSpec (spec) where

import           Camfort.Analysis hiding (describe)
import           Camfort.Analysis.Simple
import           Camfort.Analysis.TestUtils
import           Control.Lens
import           Language.Fortran.AST (ProgramUnitName(..))
import           System.FilePath ((</>))
import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

spec :: Test.Spec
spec = do
  let f n = testInputSources $ fixturesDir </> "implicit-none" </> n
  let g a = (generalizePureAnalysis . checkImplicitNone a)
  describe "implicitNone" $ do
    it "basic" $ do
      testSingleFileAnalysis (f "implicitnone1.f90") (g False) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "s"]
    it "misplaced statements" $ do
      testSingleFileAnalysis (f "implicitnone2.f90") (g False) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "implicitnone1", Named "implicitnone2"]
    it "interfaces are separate scope" $ do
      testSingleFileAnalysis (f "implicitnone3.f90") (g False) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "s"]
  describe "implicitNoneAll" $ do
    it "basic" $ do
      testSingleFileAnalysis (f "implicitnone1.f90") (g True) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "f", Named "s"]
    it "misplaced statements" $ do
      testSingleFileAnalysis (f "implicitnone2.f90") (g True) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "implicitnone1", Named "f", Named "implicitnone2", Named "f"]
    it "interfaces are separate scope" $ do
      testSingleFileAnalysis (f "implicitnone3.f90") (g True) $ \ report ->
        let ImplicitNoneReport ls = (report ^?! arResult . _ARSuccess) in
        map fst ls `shouldBe` [Named "s"]

fixturesDir :: FilePath
fixturesDir = "tests" </> "fixtures"
