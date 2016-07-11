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
   let file1 = "tests/Camfort/Specification/Units/ex1.f90"
   ps <- runIO $ readParseSrcDir file1 []
   let [(fname1, program1)] = map modifyAST ps
   let (report, ps') = let ?solver = Custom
                           ?assumeLiterals = Unitless
                       in inferUnits ("ex1.f90", program1)
   it "(0) - ex1.f90 infer" $
      report
       `shouldBe`
         "ex1.f90: Added 0 non-unitless annotation: \n\
         \ex1.f90: checked/inferred 9 user variables\n"

   expect1 <- runIO $ readFile "tests/Camfort/Specification/Units/ex1.exp.f90"
   it "(0) ex1.f90 comapre out" $
       (snd . head $ mkOutputFile ps [ps']) `shouldBe` expect1

------

   let file2 = "tests/Camfort/Specification/Units/ex2.f90"
   ps <- runIO $ readParseSrcDir file2 []
   let [(fname2, program2)] = map modifyAST ps
   let (report, ps') = let ?solver = Custom
                           ?assumeLiterals = Unitless
                       in inferUnits ("ex2.f90", program2)
   it "(1) - ex2.f90 infer" $
      report
        `shouldBe`
         "ex2.f90: Added 1 non-unitless annotation: m / s**2\n\
         \ex2.f90: checked/inferred 4 user variables\n"

   expect2 <- runIO $ readFile "tests/Camfort/Specification/Units/ex2.exp.f90"
   it "(0) ex2.f90 comapre out" $
       (snd . head $ mkOutputFile ps [ps']) `shouldBe` expect2