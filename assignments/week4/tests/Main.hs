module Main where


  import MapTest(mapTest)
  import LangTest(langTest)
  import HigherOrderTest(higherOrderTest)
  import System.Environment
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
    
  main = 
      do 
          setEnv "TASTY_TIMEOUT" "5s"
          setEnv "TASTY_QUICKCHECK_TESTS" "20"
          setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
          defaultMain allTests
          unsetEnv "TASTY_TIMEOUT"
          unsetEnv "TASTY_QUICKCHECK_TESTS"
          unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"
  
  allTests = testGroup "all tests" [
        mapTest,
        langTest,
        higherOrderTest
        
      ]
  