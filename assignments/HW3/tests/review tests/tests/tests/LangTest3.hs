{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}


module LangTest3 where


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

import Lang3 (Ast(..), eval)

import Lang3Parser (parser)


arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- arbitrary
                                 str <- elements ["x","y","z"]
                                 elements [LiteralInt i, Var str]
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
                                     str <- elements ["x","y","z"]
                                     i <- arbitrary
                                     node <- elements [Assign str r, Var str, Plus l r, LiteralInt i, Separator l r]
                                     return node

deriving instance Generic Ast 
deriving instance Eq Ast 
    
instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst
-- shrink = genericShrink  -- generic shrinker can induce unexpected behaviour, like empty variables


langTest3 = testGroup "Lang Parser test" [
      QC.testProperty "Lang 3: show should match parse" $ ((\ x -> Just (x , "") == (parser $ show x)) :: Ast -> Bool)
      ]
