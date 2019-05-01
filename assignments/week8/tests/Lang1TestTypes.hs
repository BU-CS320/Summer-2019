{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Lang1TestTypes where

    import Lang1 (Ast(..), eval)
    import Test.Tasty.QuickCheck

    arbitraryZeroOneTwo :: Gen Ast
    arbitraryZeroOneTwo = elements [LiteralInt 0, LiteralInt 1, LiteralInt 2]

    arbitrarySizedAst1 ::  Int -> Gen Ast
    arbitrarySizedAst1 m | m < 1 = do i <- arbitrary
                                      elements [LiteralInt i]                      
    arbitrarySizedAst1 m | otherwise = do l <- arbitrarySizedAst1 (m `div` 2)
                                          r <- arbitrarySizedAst1 (m `div` 2)
                                          i <- arbitrary
                                          j <- arbitraryZeroOneTwo
                                          node <- elements [LiteralInt i, Plus l r, Div l j, Div l r, Sub l r, Mult l r]
                                          return node

    deriving instance Eq Ast 

    instance Arbitrary Ast where
        arbitrary = sized arbitrarySizedAst1
        shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
        shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
        shrink (Sub x y) = [x,y] ++ [Sub x' y' | (x', y') <- shrink (x, y)]
        shrink (Div x y) = [x,y] ++ [Div x' y' | (x', y') <- shrink (x, y)]
        shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]