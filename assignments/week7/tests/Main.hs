module Main where

  import System.Environment
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

  import MonadTest (monadTest)
  import Lang1Test(lang1Test)
  import Lang2Test(lang2Test)

  main = 
    do 
        setEnv "TASTY_TIMEOUT" "10s"
        setEnv "TASTY_QUICKCHECK_TESTS" "1000" --TODO: I never trust less than 10000
        setEnv "TASTY_QUICKCHECK_MAX_SIZE" "100"
        defaultMain allTests
        unsetEnv "TASTY_TIMEOUT"
        unsetEnv "TASTY_QUICKCHECK_TESTS"
        unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

  allTests = testGroup "all tests" [
      monadTest,
      lang1Test,
      lang2Test
    ]
