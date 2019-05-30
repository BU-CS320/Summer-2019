{-# LANGUAGE ScopedTypeVariables #-}
module MonadTest where


import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck
import Prelude hiding (Maybe, Either)

import MonadTestType
import BareBonesLastTestBase

import BareBonesLast
import PrinterMonad

tests = testGroup "Monad tests" [
		listLeftTest,
		listRightTest,
		listAssociationTest,
		maybeLeftTest,
		maybeRightTest,
		maybeAssociationTest,
		eitherLeftTest,
		eitherRightTest,
		eitherAssociativityTest,
		identityLeftTest,
		identityAssociativityTest,
		trivalLeftTest,
		trivalRightTest,
		trivalAssociativityTest,
		printerLeftTest,
		printerRightTest, --TODO: move these to a proper place
		printerAssociativityTest
	]

listLeftTest = testProperty "left identity for List Monad: forall n :: Integer, f :: Integer -> List Integer. (return n >>= f) == (f n)" $
	\(n :: Integer) (Fun _ f :: Fun Integer (List Integer)) -> (return n >>= f) == (f n)

listRightTest = testProperty "right identity for List Monad: forall l :: List Integer. (lst >>= return) == lst" $
	\(lst :: List Integer) -> (lst >>= return) == lst

listAssociationTest = testProperty "accociativity for List Monad: forall l: List Integer, f1 :: Fun Integer (List Bool), f2 :: Fun Bool (List Integer). (lst >>= f1 >>= f2 ) == (lst >>= (\\x -> f1 x >>= f2))" $
	\(lst :: List Integer) (Fun _ f1 :: Fun Integer (List Bool)) (Fun _ f2 :: Fun Bool (List Integer))-> 
		(lst >>= f1 >>= f2 ) == (lst >>= (\x -> f1 x >>= f2)) 

--maybe test
maybeLeftTest = testProperty "left identity law for Maybe Monads: forall m:: Maybe Integer, function f :: Fun Integer (Maybe Integer), (return m >>= f ) == (f m)" $
	\(m::Integer) (Fun _ f :: Fun Integer (Maybe Integer)) ->  (return m >>= f) ==  (f m)

maybeRightTest = testProperty "right identity law for Maybe Monads: forall m::Maybe Integer, (m >>= return) == m" $
	\(m::Maybe Integer) -> (m >>= return) == m

maybeAssociationTest = testProperty "accociativity for Maybe Monad: forall m::Maybe Integer, f1 :: Integer -> Maybe Integer, f2 :: Integer -> Maybe Bool. (m >>= f1 >>= f2) == (m >>= (\\x -> f1 x >>= f2))" $
	\(m::Maybe Integer) (Fun _ f1 :: Fun Integer (Maybe Integer)) (Fun _ f2 :: Fun Integer (Maybe Bool)) ->
		(m >>= f1 >>= f2) == (m >>= (\x -> f1 x >>= f2))

-- --Either test
eitherLeftTest = testProperty "left identity law for Either Monads: forall e:: Integer, f :: Integer -> Either Bool Integer. (return e >>= f) == f e" $
	\(e::Integer) (Fun _ f :: Fun (Integer) (Either Bool Integer)) -> (return e >>= f) == f e

eitherRightTest = testProperty "right identity law for Either Monads: forall e::Either Bool Integer, (e >>= return) == e" $
	\(e::(Either Bool Integer)) -> (e >>= return) == e

eitherAssociativityTest = testProperty "accociativity for Either Monad: forall e::Either Bool Bool, f1 :: Bool -> Either Bool Integer, f2 :: Integer -> Either Bool Bool. (e >>= f1 >>= f2) == (e >>= (\\x -> f1 x >>= f2))" $
	\(e::Either Bool Bool) (Fun _ f1:: Fun Bool (Either Bool Integer)) (Fun _ f2:: Fun Integer (Either Bool Bool)) ->
		(e >>= f1 >>= f2) == (e >>= (\x -> f1 x >>= f2)) 

-- --Identity tests

identityLeftTest = testProperty "left identity law for Identity Monads: forall i:: Integer, f :: Integer -> Identity Integer. (return i >>= f) == f i" $
	\(i::Integer) (Fun _ f:: Fun Integer (Identity Integer) )-> (return i >>= f) == f i

identityRightTest = testProperty "right identity law for Identity Monads: forall i::Identity Integer, (i >>= return) == i" $
	\(i::Identity Integer) -> (i >>= return) == i

identityAssociativityTest = testProperty "accociativity for Idenity Monad: forall i::Idenity Bool, f1 :: Integer -> Idenity Bool, f2 :: Bool -> Idenity (List Bool). (i >>= f1 >>= f2) == (i >>= (\\x -> f1 x >>= f2))" $
	\(i::Identity Integer) (Fun _ f1:: Fun Integer (Identity Bool)) (Fun _ f2:: Fun Bool (Identity (List Bool))) ->
		(i >>= f1 >>= f2) == (i >>= (\x -> f1 x >>= f2)) 


--Trival tests
trivalLeftTest = testProperty "left identity law for Trival Monads: forall t:: Integer, f :: Integer -> Trival Integer. (return t >>= f) == f t" $
	\(t:: Integer) (Fun _ f:: Fun Integer (Trival Integer))-> (return t >>= f) == f t

trivalRightTest = testProperty "right identity law for Trival Monads: forall t::Trival Integer, (t >>= return) == t" $
	\(t:: Trival Integer) -> (t >>= return) == t

trivalAssociativityTest = testProperty "accociativity for Trival Monad: forall t::Trival Integer, f1 :: Integer -> Trival Integer, f2 :: Integer -> Trival Integer. (t >>= f1 >>= f2) == (t >>= (\\x -> f1 x >>= f2))" $
	\(t:: Trival Integer) (Fun _ f1:: Fun Integer (Trival Integer)) (Fun _ f2:: Fun Integer (Trival Integer)) ->
		(t >>= f1 >>= f2) == (t >>= (\x -> f1 x >>= f2))

--Printer Monad Tests
printerLeftTest = testProperty "left identity law for Printer Monads: forall res:: Integer, f :: Integer -> PrinterMonad Bool Integer. (return res >>= f) == f res" $
	\(res:: Integer) (Fun _ f:: Fun Integer (ShowablePrinter Bool Integer)) ->
		(return res >>= f) == (f res)

printerRightTest = testProperty "right identity law for Printer Monads: forall res::Printer Bool Integer, (res >>= return) == res" $
	\(res:: ShowablePrinter Bool Integer) -> (res >>= return) == (res)

printerAssociativityTest = testProperty "accociativity for Printer Monad: forall res::Printer Bool Integer, f1 :: Bool -> Printer Bool Integer, f2 :: Integer -> Printer Bool Integer. (res >>= f1 >>= f2) == (res >>= (\\x -> f1 x >>= f2))"$
	\(res:: ShowablePrinter Bool Integer) (Fun _ f1:: Fun Integer (ShowablePrinter Bool Integer)) (Fun _ f2:: Fun Integer (ShowablePrinter Bool Integer))-> 
		(res >>= f1 >>= f2) == (res >>= (\x -> f1 x >>= f2))
                        