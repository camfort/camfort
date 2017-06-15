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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Camfort.Transformation.EquivalenceElimSpec where

import System.FilePath
import System.Directory

import Test.Hspec

import Camfort.Transformation.EquivalenceElim
import Camfort.Functionality
import Camfort.Helpers
import Camfort.Input

samplesBase :: FilePath
samplesBase = "tests" </> "fixtures" </> "Transformation"

readExpected :: FilePath -> IO String
readExpected filename = do
  let path = samplesBase </> filename
  readFile path

readActual :: FilePath -> IO String
readActual argumentFilename = do
  let argumentPath = samplesBase </> argumentFilename
  let outFile = argumentPath `addExtension` "out"
  equivalences argumentPath [] outFile
  actual <- readFile outFile
  removeFile outFile
  return actual

spec :: Spec
spec =
  describe "Equivalence elimination test" $ do
      expected <- runIO $ readExpected "equiv.expected.f90"
      actual <- runIO $ readActual "equiv.f90"
      it "it eliminates equivalence statements" $
        actual `shouldBe` expected
      ----
      let rfun = mapM refactorEquivalences
      let infile = samplesBase </> "equiv.f90"
      report <- runIO $ doRefactor rfun infile [] "equiv.expected.f90"
      it "report is as expected" $
        report `shouldBe` expectedReport

expectedReport =
  "6:3: removed equivalence \n\
  \14:3: added copy due to refactored equivalence\n\
  \15:3: added copy due to refactored equivalence\n"
