module Main where

import System.Environment
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

import Lang0Test (lang0Test)
import Lang3Test (lang3Test)
import Lang4Test (lang4Test)
import ReaderTest (readerTest)
import StateTest (stateTest)
import Parser1Test (parser1Test) --  TODO: Should be renamed to match source
import Parser2Test (parser2Test)
import Parser3Test (parser3Test)
import Parser4Test (parser4Test)

import Lang1Test(tests) -- TODO: prefer this style to the above
import Lang2Test(tests)

import EqTest(tests)
import OrdTest(tests)
import FunctorTest(tests)
import MonadTest(tests)
import TypeclassProblemsTest(tests)

main = 
  do setEnv "TASTY_TIMEOUT" "60s"
     setEnv "TASTY_QUICKCHECK_TESTS" "100" 
     setEnv "TASTY_QUICKCHECK_MAX_SIZE" "100"
     defaultMain allTests
     unsetEnv "TASTY_TIMEOUT"
     unsetEnv "TASTY_QUICKCHECK_TESTS"
     unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

allTests = testGroup "all tests" [
    testGroup "TypeclassProblems tests" [TypeclassProblemsTest.tests],
    testGroup "BareBonesLast tests" [EqTest.tests, OrdTest.tests, FunctorTest.tests, MonadTest.tests],
    testGroup "Reader tests" [readerTest],
    testGroup "State tests" [stateTest],
	testGroup "Lang1 tests" [Lang1Test.tests],
	testGroup "Lang2 tests" [Lang2Test.tests],
    testGroup "Lang3Test" [lang3Test],
    testGroup "Lang4Test" [lang4Test],
    testGroup "Lang0Parser tests" [lang0Test],
    testGroup "Lang1Parser tests" [parser1Test],
    testGroup "Lang2Parser tests" [parser2Test],
    testGroup "Lang3Parser tests" [parser3Test],
    testGroup "Lang4Parser tests" [parser4Test]
  ]
