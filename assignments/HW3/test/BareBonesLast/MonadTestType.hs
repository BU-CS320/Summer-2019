{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module MonadTestType where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.Monad

import PrinterMonad


data ShowablePrinter out res = ShowablePrinter [out] res deriving Show 

showableToPrinter :: ShowablePrinter out res -> PrinterMonad out res 
showableToPrinter (ShowablePrinter plist res) = PrinterMonad plist res

printerToShowable :: PrinterMonad out res -> ShowablePrinter out res 
printerToShowable (PrinterMonad plist res) = ShowablePrinter plist res

mapFuncRes :: (a -> b) -> (c -> a) -> (c -> b)
mapFuncRes f g input = f $ g input

deriving instance (Eq out, Eq res) => Eq (PrinterMonad out res)
deriving instance (Eq out, Eq res) => Eq (ShowablePrinter out res)

instance Functor (ShowablePrinter out) where 
	fmap f a = printerToShowable $ fmap f (showableToPrinter a)

instance Applicative (ShowablePrinter out) where 
	pure = return
	(<*>) = ap

instance Monad (ShowablePrinter out) where 
	a >>= f = printerToShowable $ (showableToPrinter a) >>= (mapFuncRes showableToPrinter f)
	return a = printerToShowable $ return a

instance (Arbitrary out, Arbitrary res) => Arbitrary (ShowablePrinter out res) where
	arbitrary = do pList <- arbitrary; res <- arbitrary; pure $ ShowablePrinter pList res
	shrink (ShowablePrinter pList res) = 
		[ShowablePrinter pList smallerRes | smallerRes <- shrink res] ++
		[ShowablePrinter smallerPList res | smallerPList <- shrink pList] ++
		[ShowablePrinter smallerPList smallerRes | smallerPList <- shrink pList, smallerRes <- shrink res]
    
