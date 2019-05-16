{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Lang2TestTypes (Ast(..), eval) where 

    import Lang2 (Ast(..), eval)
    import Test.Tasty.QuickCheck


    arbitrarySizedAst2 ::  Int -> Gen Ast
    arbitrarySizedAst2 m | m < 1 = do i <- arbitrary
                                      elements [LiteralInt i]                    
    arbitrarySizedAst2 m | otherwise = do l <- arbitrarySizedAst2 (m `div` 2)
                                          r <- arbitrarySizedAst2 (m `div` 2)
                                          i <- arbitrary
                                          node <- elements [LiteralInt i, Plus l r, Separator l r, Print l, Sub l r, Mult l r]
                                          return node

    deriving instance Eq Ast 

    instance Arbitrary Ast where
        arbitrary = sized arbitrarySizedAst2
        shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
        shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
        shrink (Sub x y) = [x,y] ++ [Sub x' y' | (x', y') <- shrink (x, y)]
        shrink (Separator x y) = [x,y] ++ [Separator x' y' | (x', y') <- shrink (x, y)]
        shrink (Print x) = [x] ++ [Print x' | x' <- shrink x]
        shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]
