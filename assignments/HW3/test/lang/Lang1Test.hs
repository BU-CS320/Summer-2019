{-# LANGUAGE StandaloneDeriving #-}
module Lang1Test where

import Lang1  (Ast(..), eval)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import qualified Data.Map as Map


import Test.Tasty.QuickCheck


arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = 
  do i <- arbitrary
     pure $ LiteralInt i                
arbitrarySizedAst m | otherwise =
  do l <- arbitrarySizedAst (m `div` 2)
     r <- arbitrarySizedAst (m `div` 2)
     i <- arbitrary
     elements [LiteralInt i, Plus l r, Sub l r, Mult l r,Div l r]
     

deriving instance Eq Ast 

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst
    shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
    shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
    shrink (Sub x y) = [x,y] ++ [Sub x' y' | (x', y') <- shrink (x, y)]
    shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]
    shrink (Div x y) = [x,y] ++ [Div x' y' | (x', y') <- shrink (x, y)]



safeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
safeAdd (Just x) (Just y) = Just (x + y)
safeAdd _ _ = Nothing

safeSub :: Maybe Integer -> Maybe Integer -> Maybe Integer
safeSub (Just x) (Just y) = Just (x - y)
safeSub _ _ = Nothing

safeDiv :: Maybe Integer -> Maybe Integer -> Maybe Integer
safeDiv (Just x) (Just 0) = Nothing
safeDiv (Just x) (Just y) = Just (x `div` y)
safeDiv _ _ = Nothing

safeMult :: Maybe Integer -> Maybe Integer -> Maybe Integer
safeMult (Just x) (Just y) = Just (x * y)
safeMult _ _ = Nothing

--TODO: better messages
tests = testGroup "Test lang 1" [
  testProperty "for all integer i, evaluation of LiteralInt i should be Just i." $
                \i -> eval (LiteralInt i) == (Just i),
  testProperty "For all ast1 & ast2, `Plus ast1 ast2` should evaluate to `(eval ast1) + (eval ast2)`" $
                \ast1 ast2 -> eval (Plus ast1 ast2) == safeAdd (eval ast1) (eval ast2),
  testProperty "For all ast1 & ast2, `Sub ast1 ast2` should evaluate to `(eval ast1) - (eval ast2)`" $
                \ast1 ast2 -> eval (Sub ast1 ast2) == safeSub (eval ast1) (eval ast2),
  testProperty "For all ast1 & ast2, `Mult ast1 ast2` should evaluate to `(eval ast1) * (eval ast2)`" $
                \ast1 ast2 -> eval (Mult ast1 ast2) == safeMult (eval ast1) (eval ast2),
  testProperty "For all ast1 & ast2, `Div ast1 ast2` should evaluate to `(eval ast1) / (eval ast2)` where divide by zero returns Nothing" $
                \ast1 ast2 -> eval (Div ast1 ast2) == safeDiv (eval ast1) (eval ast2),
  testProperty "eval anything / 0    returns Nothing" $
                \ast -> eval (ast `Div` LiteralInt 0) == Nothing,
  testCase  "div by 0 never recovers" $
                assertEqual [] Nothing $ eval $ (LiteralInt 1)  `Plus` (LiteralInt 1) `Mult` (LiteralInt 1 `Div` ((LiteralInt 1 `Div` LiteralInt 0) `Div` LiteralInt 1)) `Mult` (LiteralInt 1) `Plus` (LiteralInt 1) 
--testProperty "For all ast, if any of the sub expression is Nothing, the ast should evaluate to nothing" $
--                   \ast -> (any (== Nothing) . map eval $ (subExpression ast)) ==> (eval ast == Nothing)
  ]
