module ParseTest where
  
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
  import ParserMonad (parse)
  import LambdaCalcParser (parser)
  import TestShowTermType
  
  parsetest = testGroup "parse test" [
    testProperty "for all lambda string, the parser should parse it correctly" $
      \showTerm -> 
        (parse parser $ showPretty showTerm 100) == Just (showTermToHw showTerm, "")
    ]