module FunctorTest where

import Prelude hiding (Maybe, Either)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

import BareBonesLastTestBase

import BareBonesLast (List, Either, Trival, Maybe, Identity, Pair)

tests = testGroup "Test for functor" [
		listFunctorLaw1Test,
		listFunctorLaw2Test,
		pairFunctorLaw1Test,
		pairFunctorLaw2Test,
		maybeFunctorLaw1Test,
		maybeFunctorLaw2Test,
		eitherFunctorLaw1Test,
		eitherFunctorLaw2Test,
		identFuntorLaw1Test,
		identFuntorLaw2Test,
		trivalTest,
		trivalTest2
	]

	-- TODO: make laws more explicit, and remove uneeded annotations

listFunctorLaw1Test = QC.testProperty "For all Lists l, if id function is applied to the list then the result should be the same list" $
	\la -> (fmap id (la::(List Integer))) == (la::(List Integer))

listFunctorLaw2Test = QC.testProperty "For all Lists l, fmap (f . g) l == (fmap f . fmap g) l" $ 
	\la -> (fmap ((+1) . (*2)) (la::(List Integer))) == ((fmap (+1) . fmap (*2)) (la::(List Integer)))
		
pairFunctorLaw1Test = QC.testProperty "For all paris p, fmap id p should equal p" $
	\p -> (fmap id (p::(Pair Integer Integer))) == (p::Pair Integer Integer)

pairFunctorLaw2Test = QC.testProperty "For all pairs p, fmap (f . g) p == (fmap f . fmap g) p" $
	\p -> (fmap ((+1) . (*2)) p::(Pair Integer Integer)) == ((fmap (+1) . fmap (*2)) p::(Pair Integer Integer))

maybeFunctorLaw1Test = QC.testProperty "For all maybe types m, fmap id m should equal m" $
	\m -> (fmap id m::(Maybe Integer)) == (m::(Maybe Integer))

maybeFunctorLaw2Test = QC.testProperty "For all maybe types m, fmap (f . g) m == (fmap f . fmap g) m" $
	\m -> (fmap ((+1) . (*2)) m::(Maybe Integer)) == ((fmap (+1) . fmap(*2)) m::(Maybe Integer))

eitherFunctorLaw1Test = QC.testProperty "For all either types e, fmap id e should equal e" $
	\e -> (fmap id e::(Either Integer Integer)) == (e::(Either Integer Integer))

eitherFunctorLaw2Test = QC.testProperty "For all either types e, fmap (f . g) e == (fmap f . fmap g) e" $
	\e -> (fmap ((+1) . (*2)) e::(Either Integer Integer)) == ((fmap (+1) . fmap(*2)) e::(Either Integer Integer))

identFuntorLaw1Test = QC.testProperty "For all identity types i, fmap id i == i" $
	\i -> (fmap id i::(Identity Integer)) == (i::Identity Integer)

identFuntorLaw2Test = QC.testProperty "For all idenity types i, fmap (f . g) i == (fmap g . fmap f) i" $
	\i -> (fmap ((+1) . (*2)) i::(Identity Integer)) == ((fmap (+1) . fmap (*2)) i::(Identity Integer))


trivalTest = QC.testProperty "First functor law test for trival" $ 
	\x -> (fmap id x::(Trival Integer)) == (x::(Trival Integer))

trivalTest2 = QC.testProperty "Second functor law test for trival" $
	\x -> (fmap ((+1) . (*2)) x::(Trival Integer)) == ((fmap (+1) . fmap (*2)) x::(Trival Integer))

