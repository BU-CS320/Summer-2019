module Parser4Test where

  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
  import HelpShow
  import Lang4 (Ast(..), eval, showFullyParen)
  import Lang4TestTypes
  import Lang4Parser (parser)
  import ParserMonad (parse)

  showPretty :: Ast -> Integer -> String
  showPretty (LiteralInt i)     _ = show i
  showPretty (Var s)            _ = s
  showPretty (l `Mult` r)       d = parenthesize d 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0))
  showPretty (l `Plus` r)       d = parenthesize d 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
  showPretty (l `Sub` r)        d = parenthesize d 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2))
  showPretty (Let s val inThis) d = parenthesize d 5  ("let " ++ s ++ " = " ++ showPretty val 4 ++ " in " ++ showPretty inThis 5 ) -- binds most weakly

  parser4Test = testGroup "Lang4 Parser test" [
      testProperty "Lang 4: showFullyParen should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
      testProperty "Lang 4: showPretty should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showPretty x 100)) :: Ast -> Bool)
    ]