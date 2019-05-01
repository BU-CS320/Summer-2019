module Main where

import Test.Tasty

import CheckTest
import EvalTest
import ParserTest

main = defaultMain testSuite


testSuite =
  testGroup
    "allTests"
    [
    CheckTest.tests,
    EvalTest.tests,
    ParserTest.tests
    -- ...
    ]
