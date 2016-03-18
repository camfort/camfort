import Test.HUnit

import TransformationTests.Common
--import AnalsisTests.Stencils.TwoDimensional

main :: IO ()
main = do -- Common blcock tests
          runTestTT (TestList [test toArgsIntegration, test toArgsIntegration2, test (assertEqual "FALSE" 0 1)])
          -- Stencil spec tests
          --runTestTT twoDimensionalTests
          return ()
       