{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- TODO: is a mess and should be cleaned

module EqTest where

import Prelude hiding (List(..), Pair(..),Maybe(..),Either(..),)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Arbitrary.Generic
import GHC.Generics (Generic)

import BareBonesLastTestBase

import BareBonesLast

tests = testGroup "Test for Eq" [
	listTest,
	maybeTest,
	pairTest,
	eitherTest,
	idTest, 
	triTest
	]
		
-- List
listTest = testGroup "Test equal List" [

  QC.testProperty "Eq Reflexivity List a == List a = True" $ 
	 \m -> m == (m :: List Bool),
	  
  QC.testProperty "Eq Symmetry List (x==y) == (y==x)" $ 
	 \x y -> ((x:: List Bool) == (y :: List Bool)) == (y==x),

  QC.testProperty "Eq Negation List x /= y = not (x == y)" $ 
	 \x y -> (x /= y) == not ((x :: List Bool) == (y:: List Bool))          
  ]

f :: List () -> List ()
f Nil = (Cons () Nil)
f (Cons a b) = Cons () ((Cons a b))

fb :: List Bool -> List Bool
fb Nil = (Cons False Nil)
fb (Cons a b) = Cons False ((Cons a b))


maybeTest = testGroup "Test equal Maybe" [

  QC.testProperty "Eq Reflexivity Maybe a == Maybe a = True" $ 
	 \m -> m == (m ::  Maybe Bool),
  
  QC.testProperty "Eq Symmetry Maybe (x==y) == (y==x)" $ 
	 \x y -> ((x::  Maybe Bool) == (y ::  Maybe Bool))==(y==x),
		  
  QC.testProperty "Eq Transitivity Maybe if x==y and y==z then x==z" $ 
	 \x y z -> (((x::  Maybe ())==(y ::  Maybe ())) && (y==(z ::  Maybe ()))) ==> x==z,

  QC.testProperty "Eq Substitutivity Maybe if a == b, then f a == f b" $ 
	 \x y -> ((x ::  Maybe Bool) == (y ::  Maybe Bool)) ==> (f2 x) == (f2 y),

  QC.testProperty "Eq Negation Maybe x /= y = not (x == y)" $ 
	 \x y -> (x /= y) == not ((x ::  Maybe Bool) == (y::  Maybe Bool))          
  ]

f2 ::  Maybe Bool ->  Maybe Bool
f2  Nothing =  Nothing
f2 (Just a) =  Just (not a)

--
pairTest = testGroup "Test equal Pair" [

  QC.testProperty "Eq Reflexivity Pair a == Pair a = True" $ 
	 \m -> m == (m :: Pair Bool Bool),
  
  QC.testProperty "Eq Symmetry Pair (x==y) == (y==x)" $ 
	 \x y -> ((x:: Pair Bool Bool) == (y :: Pair Bool Bool)) == (y==x),

  QC.testProperty "Eq Transitivity Pair if x==y and y==z then x==z" $ 
	 \x y z -> (((x:: Pair Bool ())==(y :: Pair Bool ())) && (y==(z :: Pair Bool ())))==> (x==z),
	   
  QC.testProperty "Eq Substitutivity Pair if a == b, then f a == f b" $ 
	 \x y -> ((x :: Pair Bool Bool) == (y :: Pair Bool Bool)) ==> (f3 x) == (f3 y), 

  QC.testProperty "Eq Negation Pair x /= y = not (x == y)" $ 
	 \x y -> (x /= y) == not ((x :: Pair Bool Bool) == (y:: Pair Bool Bool))          
  ]

f3 :: Pair Bool Bool -> Pair Bool Bool
f3 (Pair a b) = Pair (not a) (not b)

--
idTest = testGroup "Test equal Id" [

  QC.testProperty "Eq Reflexivity Id a == Id a = True" $ 
	 \m -> m == (m :: Identity Bool),
  
  QC.testProperty "Eq Symmetry Id (x==y) == (y==x)" $ 
	 \x y -> ((x:: Identity Bool) == (y :: Identity Bool)) == (y==x),

  QC.testProperty "Eq Transitivity Id if x==y and y==z then x==z" $ 
	 \x y z -> (((x:: Identity Bool)==(y :: Identity Bool)) && (y==(z :: Identity Bool))) ==> (x==z),

  QC.testProperty "Eq Substitutivity Id if a == b, then f a == f b" $ 
	 \x y -> ((x :: Identity Bool) == (y :: Identity Bool)) ==> (f4 x) == (f4 y),

  QC.testProperty "Eq Negation Id x /= y = not (x == y)" $ 
	 \x y -> (x /= y) == not ((x :: Identity Bool) == (y:: Identity Bool))          
  ]

f4 :: Identity Bool -> Identity Bool
f4 (Identity a) = Identity (not a)

--
eitherTest = testGroup "Test equal Either" [

  QC.testProperty "Eq Reflexivity Either a == Either a = True" $ 
	 \m -> m == (m ::  Either Bool Bool),
  
  QC.testProperty "Eq Symmetry Either (x==y) == (y==x)" $ 
	 \x y -> ((x::  Either Bool Bool) == (y ::  Either Bool Bool)) == (y==x),

  QC.testProperty "Eq Transitivity Either if x==y and y==z then x==z" $ 
	 \x y z -> (((x::  Either Bool ())==(y ::  Either Bool ())) && (y==(z ::  Either Bool ()))) ==> (x==z),

  QC.testProperty "Eq Substitutivity Either if a == b, then f a == f b" $ 
	 \x y -> ((x ::  Either Bool Bool) == (y ::  Either Bool Bool)) ==> ((f5 x) == (f5 y)),
			
  QC.testProperty "Eq Negation Either x /= y = not (x == y)" $ 
	 \x y -> (x /= y) == not ((x ::  Either Bool Bool) == (y::  Either Bool Bool))          
  ]

f5 ::  Either Bool Bool ->  Either Bool Bool
f5 ( Left a) =  Left (not a)
f5 ( Right b) =  Right (not b)

--
triTest = testGroup "Test equal Trival" [

  QC.testProperty "Eq Reflexivity Trival a == Trival a = True" $ 
	 \m -> m == (m :: Trival Bool),
  
  QC.testProperty "Eq Symmetry Trival (x==y) == (y==x)" $ 
	 \x y -> ((x:: Trival Bool) == (y :: Trival Bool))==(y==x),
		  
  QC.testProperty "Eq Transitivity Trival if x==y and y==z then x==z" $ 
	 \x y z -> (((x:: Trival Bool)==(y :: Trival Bool)) && (y==(z :: Trival Bool))) ==> (x==z),

  QC.testProperty "Eq Substitutivity Trival if a == b, then f a == f b" $ 
	 \x y -> ((x :: Trival Bool) == (y :: Trival Bool)) ==> ((f6 x) == (f6 y)),

  QC.testProperty "Eq Negation Trival x /= y = not (x == y)" $ 
	 \x y -> (x /= y) == not ((x :: Trival Bool) == (y:: Trival Bool))          
  ]

f6 :: Trival Bool -> Trival Bool
f6 NoA = NoA

