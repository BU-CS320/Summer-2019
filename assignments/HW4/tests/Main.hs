module Main where

  import System.Environment
  import Test.Tasty (defaultMain, testGroup, TestTree)

  import LangParserTest (parserTest)
  import LangEvalTest (errorTest, evalTest, stdLibTest)
  import EnvUnsafeTest (monadLawTest, functorLawTest)
  import UsingLambdaCalcTest (usingLambdaCalcTest)
  main = 
    do 
        setEnv "TASTY_TIMEOUT" "120s"
        setEnv "TASTY_QUICKCHECK_TESTS" "1000" --TODO: I never trust less than 10000
        setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
        defaultMain allTests
        unsetEnv "TASTY_TIMEOUT"
        unsetEnv "TASTY_QUICKCHECK_TESTS"
        unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

  allTests =
    testGroup
      "allTests"
      [
        testGroup "LangParserTest" [parserTest],
        testGroup "LangEvalTest" [errorTest, evalTest, stdLibTest],
        testGroup "EnvUnsafeTest" [monadLawTest, functorLawTest],
        testGroup "UsingLambdaCalcTest" [usingLambdaCalcTest]
      ]
