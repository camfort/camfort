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

import           Control.Arrow                          (second)
import           Control.Monad                          (forM_)
import           System.Directory
import           System.FilePath
import           System.IO.Silently                     (capture_)

import           Control.Lens

import           Test.Hspec

import qualified Language.Fortran.Util.Position         as FU

import           Camfort.Analysis                       hiding (describe)
import           Camfort.Analysis.Logger                hiding (describe)
import           Camfort.Analysis.ModFile               (MFCompiler,
                                                         genModFiles,
                                                         simpleCompiler)
import           Camfort.Analysis.TestUtils
import           Camfort.Functionality
import           Camfort.Input
import           Camfort.Transformation.EquivalenceElim

samplesBase :: FilePath
samplesBase = "tests" </> "fixtures" </> "Transformation"

readExpected :: FilePath -> IO String
readExpected filename = do
  let path = samplesBase </> filename
  readFile path

readActual :: FilePath -> IO String
readActual argumentFilename = do
  let argumentPath = samplesBase </> argumentFilename
      outFile = argumentPath `addExtension` "out"

      env = CamfortEnv
        { ceInputSources = argumentPath
        , ceIncludeDir = Nothing
        , ceExcludeFiles = []
        , ceLogLevel = LogInfo
        }

  equivalences outFile env
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

      let infile = samplesBase </> "equiv.f90"

          input = testInputSources infile & tiIncludeDir .~ Just infile

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

              spanMatches (pl, pc) (FU.SrcSpan (FU.Position _ pc1 pl1 _) (FU.Position _ pc2 pl2 _)) =
                pl == pl1 && pl == pl2 &&
                pc == pc1 && pc == pc2

              matchesExpected (expectedSpan, expectedText) message =
                spanMatches expectedSpan (message ^?! lmOrigin . _Just . oSpan) &&
                expectedText == message ^. lmMsg

          (putStrLn . show $ infoLogs)

          forM_ (zip infoLogs expectedLogs) $ \(message, expectedMessage) ->
            message `shouldSatisfy` matchesExpected expectedMessage
