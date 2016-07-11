{-# LANGUAGE ImplicitParams #-}

module Camfort.Specification.UnitsSpec (spec) where

import Camfort.Input
import Camfort.Functionality
import Camfort.Specification.Units
import Camfort.Specification.Units.Environment

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Unit specifications" $ do
         describe "Integration tests" integration


integration = do
   doIntegration 0 "ex1.f90"
      "ex1.f90: Added 0 non-unitless annotation: \n\
      \ex1.f90: checked/inferred 9 user variables\n"

   doIntegration 1 "ex2.f90"
      "ex2.f90: Added 1 non-unitless annotation: m / s**2\n\
      \ex2.f90: checked/inferred 4 user variables\n"

   doIntegration 2 "ex3.f90"
      "ex3.f90: Added 1 non-unitless annotation: m / s**2\n\
      \ex3.f90: checked/inferred 6 user variables\n"

   doIntegration 2 "param.f90"
      "param.f90: Added 2 non-unitless annotation: m / s\n\
      \param.f90: checked/inferred 5 user variables\n"

doIntegration c fname expInfer = do
   let file = "tests/Camfort/Specification/Units/" ++ fname
   ps <- runIO $ readParseSrcDir file []
   let [(fname1, program1)] = map modifyAST ps
   let (report, ps') = let ?solver = Custom
                           ?assumeLiterals = Unitless
                       in inferUnits (fname, program1)
   it ("(" ++ show c ++ ") - " ++ fname ++ " infer") $
      report `shouldBe` expInfer

   expect <- runIO $ readFile $ "tests/Camfort/Specification/Units/exp." ++ fname
   it ("(" ++ show c ++ ") - " ++ fname ++ " comapare expected out") $
       (snd . head $ mkOutputFile ps [ps']) `shouldBe` expect
