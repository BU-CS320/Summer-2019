module Main where

import TypeProblemsTest(typeProblemTests)
import Lang0Test(lang0Tests) -- TODO : names are overly prefixed, just call it test and refer to it as Lang1Test.tests
import Lang1Test(tests)
import Lang2Test(tests)
import Lang3Test(tests)
import Lang4Test(tests)
import MapTest(tests)
import HigherOrderTest(tests)
import BareBonesAgainTest(tests)

import System.Environment
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )


main = 
  do 
	  setEnv "TASTY_TIMEOUT" "30s"
	  setEnv "TASTY_QUICKCHECK_TESTS" "1000"
	  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "200" -- turned down due to apparent performance bug in Arbitrary Map: http://hackage.haskell.org/package/QuickCheck-2.13.1/docs/src/Test.QuickCheck.Arbitrary.html#line-799
	  defaultMain allTests
	  unsetEnv "TASTY_TIMEOUT"
	  unsetEnv "TASTY_QUICKCHECK_TESTS"
	  unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

allTests = testGroup "all tests" [
  typeProblemTests,
  lang0Tests, Lang1Test.tests, Lang1Test.tests, Lang2Test.tests, Lang3Test.tests, Lang4Test.tests,
  MapTest.tests,
  HigherOrderTest.tests,
  BareBonesAgainTest.tests
  ]
