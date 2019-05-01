module Parser3Test where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

import Lang3 (Ast(..), eval, showPretty)
import Lang3TestTypes
import Lang3Parser(parser)
import ParserMonad (parse)

-- show the fully parenthesized syntax
showFullyParen :: Ast -> String
showFullyParen (LiteralInt i)    = show i
showFullyParen (Var s)           = s
showFullyParen (l `Plus` r)      = "(" ++ showFullyParen l ++ " + " ++  showFullyParen r ++ ")"
showFullyParen (l `Sub` r)       = "(" ++ showFullyParen l ++ " - " ++  showFullyParen r ++ ")"
showFullyParen (l `Mult` r)      = "(" ++ showFullyParen l ++ " * " ++  showFullyParen r ++ ")"
showFullyParen (l `Separator` r) = "(" ++ showFullyParen l ++ " ; " ++  showFullyParen r ++ ")"
showFullyParen (Assign v b)      = "(" ++ v ++ " := " ++ showFullyParen b ++ ")"



parser3Test = testGroup "Lang3 Parser test" [
    testProperty "Lang 3: showFullyParen should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
    testProperty "Lang 3: showPretty should match parse" $ ((\ x -> Just (x , "") == (parse parser $ showPretty x 100)) :: Ast -> Bool)
  ]
