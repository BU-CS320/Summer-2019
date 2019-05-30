module Parser2Test where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

import Lang2 (Ast(..), eval, showPretty, showFullyParen)
import Lang2TestTypes
import Lang2Parser(parser)
import ParserMonad (parse)


parser2Test = testGroup "Lang2 Parser test" [
    testProperty "Lang 2: showFullyParen should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
    testProperty "Lang 2: showPretty should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showPretty x 100)) :: Ast -> Bool)
  ]
