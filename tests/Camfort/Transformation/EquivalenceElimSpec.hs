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
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Transformation.EquivalenceElimSpec (spec) where

import           Camfort.Analysis hiding (describe)
import           Camfort.Analysis.Logger hiding (describe)
import           Camfort.Analysis.TestUtils
import           Camfort.Functionality
import           Camfort.Transformation.EquivalenceElim
import           Control.Lens
import           Control.Monad (forM_)
import qualified Language.Fortran.Util.Position as FU
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict as Strict
import           Test.Hspec
import           Camfort.TestUtils (normalisedShouldBe)

samplesBase :: FilePath
samplesBase = "tests" </> "fixtures" </> "Transformation"

readExpected :: FilePath -> IO String
readExpected filename = do
  let path = samplesBase </> filename
  Strict.readFile path

readActual :: FilePath -> IO String
readActual argumentFilename = do
  let argumentPath = samplesBase </> argumentFilename
      outFile = argumentPath `addExtension` "out"

      env = CamfortEnv
        { ceInputSources = argumentPath
        , ceIncludeDir = Nothing
        , ceExcludeFiles = []
        , ceLogLevel = LogInfo
        , ceSourceSnippets = False
        , ceFortranVersion = Nothing
        }

  _ <- equivalences outFile env
  actual <- Strict.readFile outFile
  removeFile outFile
  return actual

spec :: Spec
spec =
  describe "Equivalence elimination test" $ do
      expected <- runIO $ readExpected "equiv.expected.f90"
      actual <- runIO $ readActual "equiv.f90"
      it "it eliminates equivalence statements" $
        actual `normalisedShouldBe` expected

      let infile = samplesBase </> "equiv.f90"

          input = testInputSources infile

      it "log is as expected" $
        testSingleFileAnalysis input (generalizePureAnalysis . refactorEquivalences) $ \report -> do
          let infoLogs = report ^.. arMessages . traverse . _MsgInfo

              addCopyMsg = "added copy due to refactored equivalence"
              removeMsg = "removed equivalence"

              expectedLogs =
                [ ((6, 3), removeMsg)
                , ((14, 3), addCopyMsg)
                , ((15, 3), addCopyMsg)
                ]

              spanMatches (pl, pc) (FU.SrcSpan (FU.Position _ pc1 pl1 _ _) (FU.Position _ pc2 pl2 _ _)) =
                pl == pl1 && pl == pl2 &&
                pc == pc1 && pc == pc2

              matchesExpected (expectedSpan, expectedText) message =
                spanMatches expectedSpan (message ^?! lmOrigin . _Just . oSpan) &&
                expectedText == message ^. lmMsg

          (putStrLn . show $ infoLogs)

          forM_ (zip infoLogs expectedLogs) $ \(message, expectedMessage) ->
            message `shouldSatisfy` matchesExpected expectedMessage
