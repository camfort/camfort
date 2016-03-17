import Test.HUnit

import TransformationTests.Common

main :: IO ()
main = do runTestTT (TestList [test toArgsIntegration, test toArgsIntegration2, test (assertEqual "FALSE" 0 1)])
          return ()
       