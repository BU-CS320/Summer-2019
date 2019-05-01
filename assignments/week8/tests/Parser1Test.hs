module Parser1Test where

  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
  import Lang1 (Ast(..), eval, showFullyParen, showPretty)
  import Lang1TestTypes
  import Lang1Parser(parser)
  import ParserMonad (parse)

  parser1Test = testGroup "Lang1 Parser test" [
      testProperty "Lang 1: showFullyParen should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
      testProperty "Lang 1: showPretty should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showPretty x 100)) :: Ast -> Bool)
    ]