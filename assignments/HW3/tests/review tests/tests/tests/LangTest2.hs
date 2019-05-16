{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}


module LangTest2 where


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

import Lang2 (Ast(..), eval)

import Lang2Parser (parser)


avaliableVar = ["x","y","z", "test", "thishello"]

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- arbitrary
                                 elements [LiteralInt i]
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
                                     i <- arbitrary
                                     node <- elements [Print l, Plus l r, LiteralInt i, Separator l r]
                                     return node

deriving instance Generic Ast 
deriving instance Eq Ast 
    
instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst
-- shrink = genericShrink  -- generic shrinker can induce unexpected behaviour, like empty variables


langTest2 = testGroup "Lang Parser test" [
      QC.testProperty "Lang 2: show should match parse" $ ((\ x -> Just (x , "") == (parser $ show x)) :: Ast -> Bool)
      ]
