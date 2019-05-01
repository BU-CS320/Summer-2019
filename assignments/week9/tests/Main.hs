module Main where

  import System.Environment
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

  import UsingLambdaCalcTest (usingLambdaCalcTest)
  import IsValueTest (isValueTest)
  import FreeVarsTest (freeVarsTest)
  import IsClosedTest (isClosedTest)
  import EvalTest (evalTest)
  import ParseTest (parsetest)
  import EqTest (eqTest)

  main = 
    do 
      setEnv "TASTY_TIMEOUT" "60s"
      -- setEnv "TASTY_QUICKCHECK_VERBOSE" "true"
      setEnv "TASTY_QUICKCHECK_TESTS" "200" 
      setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
      defaultMain allTests
      unsetEnv "TASTY_TIMEOUT"
      unsetEnv "TASTY_QUICKCHECK_TESTS"
      unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"
      -- unsetEnv "TASTY_QUICKCHECK_VERBOSE"


  allTests =
    testGroup
      "allTests"
      [
        usingLambdaCalcTest,
        isValueTest,
        freeVarsTest,
        isClosedTest,
        evalTest,
        eqTest,
        parsetest
      ]
