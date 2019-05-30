{-# LANGUAGE StandaloneDeriving #-}
module Lang2Test where

import Data.Char

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck

import qualified Data.Map as Map


import Lang2  (Ast(..), eval)
import PrinterMonad  (PrinterMonad(..), runPrinterMonad)

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = 
  do i <- arbitrary
     pure $ LiteralInt i
arbitrarySizedAst m | otherwise =
  do l <- arbitrarySizedAst (m `div` 2)
     r <- arbitrarySizedAst (m `div` 2)
     i <- arbitrary
     elements [LiteralInt i, Plus l r, Mult l r, Separator l r, Print l]
     

deriving instance Eq Ast 

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst
    shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
    shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
    shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]
    shrink (Separator x y) = [x,y] ++ [Separator x' y' | (x', y') <- shrink (x, y)]
    shrink (Print x ) = [x] ++ [Print x' | x' <- shrink x]


-- TODO: rename, names longer than impl, questionable grammar
evalAndGetPrinting :: Ast -> [Integer]
evalAndGetPrinting = fst . runPrinterMonad . eval

evalAndGetResult :: Ast -> Integer
evalAndGetResult = snd . runPrinterMonad . eval

--TODO: better messages
tests = testGroup "Test lang 2" [

  testGroup "result works correctly" [
    testProperty "For all integer i, evaluate LiteralInt i should be evaluate to i" $
        \i -> i == (evalAndGetResult $ LiteralInt i),

    testProperty "For all ast1 and ast2, `Plus ast1 ast2` should be evaluate to `(eval ast1) + (eval ast2)`" $
        \ i j -> (evalAndGetResult $ Plus i j) == (evalAndGetResult i) + (evalAndGetResult j),

    testProperty "For all ast1 and ast2, `Mult ast1 ast2` should be evaluate to `(eval ast1) * (eval ast2)`" $
        \ i j -> (evalAndGetResult $ Mult i j) == (evalAndGetResult i) * (evalAndGetResult j),

    testProperty "For all ast1 and ast2, `Separator ast1 ast2` should be evaluate to `(eval ast2)`" $
        \ i j -> (evalAndGetResult $ Separator i j) == (evalAndGetResult j),

    testProperty "For all ast, `Print ast` should be evaluate to `(eval ast)`" $
        \ ast -> (evalAndGetResult $ Print ast) == (evalAndGetResult ast)
  ],
  testGroup "'printing' works correctly" [
    testProperty "For all integer i, eval LiteralInt i should not print anything" $
                \ i -> ((evalAndGetPrinting $ LiteralInt i)) == [],
				
    testProperty "For two expression exp1 exp2, the print list for `Plus exp1 exp2` should be the print list of exp1 ++ print list of exp2" $
        \ i j -> (evalAndGetPrinting $ Plus i j) == (evalAndGetPrinting i) ++ (evalAndGetPrinting j),

    testProperty "For two expression exp1 exp2, the print list for `Mult exp1 exp2` should be the print list of exp1 ++ print list of exp2" $
        \ i j -> (evalAndGetPrinting $ Mult i j)  == (evalAndGetPrinting i) ++ (evalAndGetPrinting j),

    testProperty "For two expression exp1 exp2, the print list for `Separator exp1 exp2` should be the print list of exp1 ++ print list of exp2" $
        \ i j -> (evalAndGetPrinting $ Separator i j)  == (evalAndGetPrinting i) ++ (evalAndGetPrinting j),

    testProperty "For expression exp, the print list for `Print exp` should be the print list of exp ++ [value of eval exp]" $
        \ exp -> (evalAndGetPrinting $ Print exp)  == (evalAndGetPrinting exp ++ [evalAndGetResult exp])
    ]
  ]
