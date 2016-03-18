module TransformationTests.Common where

import Helpers
import Test.HUnit
import Functionality

samples = "tests/TransformationTests/Common/samples/"

toArgsIntegration = do let outFile = samples ++ "toArgs.out.f90"
                       commonToArgs (samples ++ "toArgs.f90") [] outFile ()
                       expected <- readFile $ samples ++ "toArgs.expected.f90"
                       actual <- readFile $ outFile
                       assertEqual "Integration test on toArgs.f90:" expected actual

toArgsIntegration2 = do let outFile = samples ++ "toArgs2.out.f90"
                        commonToArgs (samples ++ "toArgs2.f90") [] outFile ()
                        expected <- readFile $ samples ++ "toArgs2.expected.f90"
                        actual <- readFile $ outFile
                        assertEqual "Integration test on toArgs2.f90 (cont. line version):" expected actual