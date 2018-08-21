module Camfort.Specification.DerivedDataTypeSpec (spec) where

import System.FilePath ((</>))

import Control.Lens

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import qualified Language.Fortran.AST as F
import Language.Fortran.Util.ModFile (ModFile, emptyModFiles)

import Camfort.Analysis hiding (describe)
import Camfort.Analysis.ModFile (genModFiles)
import Camfort.Input (readParseSrcDir)
import Camfort.Specification.DerivedDataType

import qualified Data.Map.Strict as M
import qualified Data.Set as S

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "DerivedDataType"

ddtInferReport :: [String] -> String -> IO DerivedDataTypeReport
ddtInferReport modNames fileName = do
  let file = fixturesDir </> fileName
      modPaths = fmap (fixturesDir </>) modNames
  modFiles <- mapM mkTestModFile modPaths
  [(pf,_)] <- readParseSrcDir modFiles file []

  let r = runIdentity $ runAnalysisT file (logOutputNone True) LogError modFiles (infer pf)
  return $ r ^?! arResult . _ARSuccess

-- | Helper for producing a basic ModFile from a (terminal) module file.
mkTestModFile :: String -> IO ModFile
mkTestModFile file = head <$> genModFiles emptyModFiles compile () file []

spec :: Test.Spec
spec = do
  describe "consistent" $ do
    describe "across modfiles" $ do
      it "consistent where labels overlap" $ do
        r <- ddtInferReport ["consistSpec1b.f90"] "consistSpec1a.f90"
        successful r `shouldBe` True
      it "starred spec overrides unstarred spec" $ do
        r <- ddtInferReport ["consistSpec2b.f90"] "consistSpec2a.f90"
        successful r `shouldBe` True
  describe "error-finding" $ do
    describe "bad dimensions" $ do
      it "dim violates min bound" $ do
        r <- ddtInferReport [] "badDim1.f90"
        map (fmap (fst . head . S.toList)) (M.toList (ddtrBDE r)) `shouldBe` [(("example_d_common", 0), 0)]
      it "dim violates max bound" $ do
        r <- ddtInferReport [] "badDim2.f90"
        map (fmap (fst . head . S.toList)) (M.toList (ddtrBDE r)) `shouldBe` [(("example_d_common", 3), 2)]
    describe "bad labelling" $ do
      it "dupped index" $ do
        r <- ddtInferReport [] "dupIndex1.f90"
        S.size (ddtrIDE r) `shouldBe` 1
      it "out-of-bounds index" $ do
        r <- ddtInferReport [] "oobIndex1.f90"
        S.size (ddtrIDE r) `shouldBe` 1
      it "dupped label" $ do
        r <- ddtInferReport [] "badLabel1.f90"
        map (fmap (fst . head . S.toList)) (M.toList (ddtrBLE r)) `shouldBe` [(("example_d_common", 2), "label1")]
    describe "inconsistent specs" $ do
      it "differing type names" $ do
        r <- ddtInferReport ["inconsistSpec1b.f90"] "inconsistSpec1a.f90"
        map (fmap S.size) (M.toList (ddtrCE r)) `shouldBe` [(("inconsist1_d_common", 2), 2)]
      it "clashing label names" $ do
        r <- ddtInferReport ["inconsistSpec2b.f90"] "inconsistSpec2a.f90"
        map (fmap S.size) (M.toList (ddtrCE r)) `shouldBe` [(("inconsist2_d_common", 2), 2)]
      it "disagreeing on label names" $ do
        r <- ddtInferReport ["inconsistSpec3b.f90"] "inconsistSpec3a.f90"
        map (fmap S.size) (M.toList (ddtrCE r)) `shouldBe` [(("inconsist3_d_common", 2), 2)]
      it "when starred specs disagree" $ do
        r <- ddtInferReport ["inconsistSpec4b.f90"] "inconsistSpec4a.f90"
        map (fmap S.size) (M.toList (ddtrCE r)) `shouldBe` [(("inconsist4_d_common", 2), 2)]
