{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module BareBonesLastTestBase where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Test.QuickCheck.Arbitrary.Generic

import BareBonesLast as BB (List(..), Pair(Pair), Maybe(Nothing, Just), Either(Left, Right), Identity(Identity), Trival(NoA))
--import TypeclassProblems (DayOfTheWeek(..), HasExample(..), AllTheThings(..))



--translate from List to Prelude
l2p :: List a -> [a]
l2p Nil = []
l2p (Cons x xs) = x:(l2p xs)

--translate from Prelude to List
p2l :: [a] -> List a
p2l [] = Nil
p2l (x:xs) = Cons x (p2l xs)

-- translate Maybe to Prelude
m2p :: BB.Maybe a -> Prelude.Maybe a
m2p BB.Nothing = Prelude.Nothing
m2p (BB.Just n) = Prelude.Just n

-- translate Prelude to Maybe
p2m :: Prelude.Maybe a -> BB.Maybe a
p2m Prelude.Nothing = BB.Nothing
p2m (Prelude.Just n) = BB.Just n

r2p :: BB.Pair a b -> (a,b)
r2p (BB.Pair a b) = (a,b)

p2r :: (a,b) -> BB.Pair a b
p2r (a,b) = BB.Pair a b


e2p :: BB.Either a b -> Prelude.Either a b
e2p (BB.Left a) = Prelude.Left a
e2p (BB.Right b) = Prelude.Right b

p2e :: Prelude.Either a b -> BB.Either a b
p2e (Prelude.Left a) = BB.Left a
p2e (Prelude.Right b) = BB.Right b

instance (Arbitrary a) => Arbitrary (BB.List a) where
	arbitrary = do ls <- arbitrary; pure $ p2l $ ls
	shrink a =  fmap p2l (shrink $ l2p a)

instance (Arbitrary a) => Arbitrary (BB.Maybe a) where
	arbitrary = do ls <- arbitrary; pure $ p2m $ ls
	shrink a =  fmap p2m (shrink $ m2p a) 

instance (Arbitrary a, Arbitrary b) =>Arbitrary (Pair a b) where
	arbitrary = do ls <- arbitrary; pure $ p2r $ ls
	shrink a =  fmap p2r (shrink $ r2p a)
	
instance (Arbitrary a, Arbitrary b) => Arbitrary (BB.Either a b) where
	arbitrary = do ls <- arbitrary; pure $ p2e $ ls
	shrink a =  fmap p2e (shrink $ e2p a) 

deriving instance Generic (Trival a)
instance (Arbitrary a) => Arbitrary (Trival a) where
	arbitrary = genericArbitrary
	shrink = genericShrink 

deriving instance Generic (Identity a)
instance (Arbitrary a) => Arbitrary (Identity a) where
	arbitrary = genericArbitrary
	shrink = genericShrink 

	{-
deriving instance Eq DayOfTheWeek
deriving instance Ord DayOfTheWeek

data Silly = A | B | C Bool deriving (Eq, Generic, Show)
instance AllTheThings Silly where 
	listOfAll = [A, B, C True, C False]
instance Arbitrary Silly where
	arbitrary = genericArbitrary
	shrink = genericShrink
	-}