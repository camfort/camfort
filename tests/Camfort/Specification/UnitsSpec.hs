module Camfort.Specification.UnitsSpec (spec) where

import qualified Data.ByteString.Char8 as B

import Camfort.Input
import Camfort.Functionality
import Camfort.Output
import Camfort.Specification.Units
import Camfort.Specification.Units.Environment

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Unit specifications" $ do
         describe "Integration tests of infer and synthesise" integration


integration = do
   doIntegration 0 "ex1.f90"
      "ex1.f90: Added 0 annotations\n\
      \ex1.f90: checked/inferred 9 user variables\n"

   doIntegration 1 "ex2.f90"
      "ex2.f90: Added 1 annotations\n\
      \ex2.f90: checked/inferred 4 user variables\n"

   doIntegration 2 "ex3.f90"
      "ex3.f90: Added 3 annotations\n\
      \ex3.f90: checked/inferred 6 user variables\n"

   doIntegration 2 "param.f90"
      "param.f90: Added 2 annotations\n\
      \param.f90: checked/inferred 5 user variables\n"

doIntegration c fname expInfer = do
   let file = "tests/Camfort/Specification/Units/" ++ fname
   ps <- runIO $ readForparseSrcDir file []
   let [(fname1, _, program1)] = ps
   let (report, ps') = let ?solver = Custom
                           ?assumeLiterals = Unitless
                       in synthesiseUnits (fname, program1)
   it ("(" ++ show c ++ ") - " ++ fname ++ " infer") $
      report `shouldBe` expInfer

   expect <- runIO $ readFile $ "tests/Camfort/Specification/Units/exp." ++ fname
   it ("(" ++ show c ++ ") - " ++ fname ++ " comapare expected out") $
       (B.unpack $ mkOutputText ("exp." ++ fname) (head $ mkOutputFileForpar ps [ps']))
          `shouldBe` expect
