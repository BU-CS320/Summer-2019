{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}


module LangTest4 where


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

import Lang4 (Ast(..), eval)

import Lang4Parser (parser)


arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- arbitrary
                                 str <- elements ["x","y","z"]
                                 elements [LiteralInt i, Var str]
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
                                     str <- elements ["x","y","z"]
                                     i <- arbitrary
                                     node <- elements [Let str l r, Var str, Plus l r, LiteralInt i]
                                     return node

deriving instance Generic Ast 
deriving instance Eq Ast 
    
instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst
    -- shrink = genericShrink  -- generic shrinker can induce unexpected behaviour, like empty variables


langTest4 = testGroup "Lang Parser test" [
      QC.testProperty "Lang 4: show should match parse" $ ((\ x -> Just (x , "") == (parser $ show x)) :: Ast -> Bool)
      ]
