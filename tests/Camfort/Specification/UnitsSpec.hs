module Camfort.Specification.UnitsSpec (spec) where

import Camfort.Input
import Camfort.Functionality

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Unit specifications" $ do
         describe "Integration tests" integration


integration = do
   let file1 = "tests/Camfort/Specification/Units/ex1.f90"
   program1 <- runIO $ readForparseSrcDir file1 []

   let file2 = "tests/Camfort/Specification/Units/ex2.f90"
   program2 <- runIO $ readForparseSrcDir file2 []
   return ()
