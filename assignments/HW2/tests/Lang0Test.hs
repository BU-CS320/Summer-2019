{-# LANGUAGE StandaloneDeriving #-}
module Lang0Test where

import Data.Char
import Lang0  (Ast(..), eval)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = 
  do i <- arbitrary
     pure $ LiteralInt i
arbitrarySizedAst m | otherwise =
  do l <- arbitrarySizedAst (m `div` 2)
     r <- arbitrarySizedAst (m `div` 2)
     i <- arbitrary
     elements [LiteralInt i, Plus l r, Mult l r]
     

deriving instance Eq Ast 

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst
    shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
    shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
    shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]


	
lang0Tests = testGroup "Test lang 0" [
  testProperty "for all integer i, evaluation of LiteralInt i should be i." $
                \i -> eval (LiteralInt i) ==  i,
  testProperty "For all ast1 & ast2, `Plus ast1 ast2` should evaluate to `(eval ast1) + (eval ast2)`" $
                \ast1 ast2 -> eval (Plus ast1 ast2) == (eval ast1) + (eval ast2),
  testProperty "For all ast1 & ast2, `Mult ast1 ast2` should evaluate to `(eval ast1) * (eval ast2)`" $
                \ast1 ast2 -> eval (Mult ast1 ast2) == (eval ast1) * (eval ast2)
  ]

