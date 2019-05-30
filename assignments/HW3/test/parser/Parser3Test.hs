module Parser3Test where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

import Lang3 (Ast(..), eval, showPretty, showFullyParen)
import Lang3TestTypes
import Lang3Parser(parser)
import ParserMonad (parse)





parser3Test = testGroup "Lang3 Parser test" [
    testProperty "Lang 3: showFullyParen should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
    testProperty "Lang 3: showPretty should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showPretty x 100)) :: Ast -> Bool)
  ]
