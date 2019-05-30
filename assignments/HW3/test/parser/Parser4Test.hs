module Parser4Test where

  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
  import Lang4 (Ast(..), eval, showFullyParen, showPretty)
  import Lang4TestTypes
  import Lang4Parser (parser)
  import ParserMonad (parse)


  parser4Test = testGroup "Lang4 Parser test" [
      testProperty "Lang 4: showFullyParen should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
      testProperty "Lang 4: showPretty should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showPretty x 100)) :: Ast -> Bool)
    ]