{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}


module Lang0Test where


import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Test.QuickCheck.Arbitrary.Generic
import Test.Tasty.QuickCheck



import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck as QC
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Test.QuickCheck.Arbitrary.Generic

import Lang0 (Ast(..), eval, showFullyParen, showPretty)

import Lang0Parser (parser)
import ParserMonad(parse)


arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- arbitrary
--                                 str <- elements ["x","y","z"]
--                                 elements [LiteralInt i, Var str]
                                 return $ LiteralInt i
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
--                                     str <- elements ["x","y","z"]
                                     i <- arbitrary
                                     elements [Plus l r, Sub l r, Mult l r, LiteralInt i]

deriving instance Generic Ast 
deriving instance Eq Ast 
    
instance Arbitrary Ast where
  arbitrary = sized arbitrarySizedAst
--  shrink = genericShrink  -- generic shrinker can induce unexpected behaviour, like empty variables, but should be fine on lang0,1,2
  shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
  shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
  shrink (Sub x y) = [x,y] ++ [Sub x' y' | (x', y') <- shrink (x, y)]
  shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]
-- see: http://hackage.haskell.org/package/QuickCheck-2.12.6.1/docs/Test-QuickCheck-Arbitrary.html#v:shrink


lang0Test = testGroup "Lang Parser test" [
      QC.testProperty "Lang 0: fully parenthesized expressions should parse" $ ((\ x -> Just (x , "") == ((parse parser ) $ showFullyParen x)) :: Ast -> Bool),
      QC.testProperty "Lang 0: expressions that are pretty printed should parse" $ ((\ x -> Just (x , "") == ((parse parser ) $ showPretty x 100)) :: Ast -> Bool)
      ]
