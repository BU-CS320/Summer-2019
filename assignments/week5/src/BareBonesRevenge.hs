module BareBonesRevenge where


import Prelude hiding (List(..), Pair(..),Maybe(..),Either(..),)

-- Implement the following type class instances over our bare bones data
-- You may add any dependencies you see fit
-- Follow the instructions, your implementations must obey the standerd type class laws.
-- type class laws will explained in lecture on W 2/30 and explored in the analytical part of this homework (if you get stuck here, look at that). 
-- Note that sometimes laws and typing rules completely define the implementation, 
-- if you don't see instructions look up the laws.


data List a = Nil | Cons a (List a) deriving Show

-- lists are equal when they have equal members in the same order
instance {- ???  => -} Eq (List a) where
-- fill it in!

-- lists are ordered lexographicly over elements
instance {- ???  => -} Ord (List a) where
-- fill it in!

instance {- ???  => -} Functor List where
-- fill it in!


data Pair a b = Pair a b deriving Show

-- Pairs are equal when they have equal members in the same order
instance {- ???  => -} Eq (Pair a b) where
-- fill it in!

-- Pair are ordered lexographicly over elements
instance {- ???  => -} Ord (Pair a b) where
-- fill it in!

-- note it could be a functor on both type args
-- keep it easy and just define it on 1
instance {- ???  => -} Functor (Pair a) where
-- fill it in!


data Maybe a = Nothing | Just a deriving Show


-- Maybe are equal if they are both Just the same thing, or both Nothing
instance {- ???  => -} Eq (Maybe a) where
-- fill it in!

-- "Nothing" is less than Just, otherwise it matches the order of a
instance {- ???  => -} Ord (Maybe a) where
-- fill it in!

instance {- ???  => -} Functor Maybe where
-- fill it in!



data  Either a b  =  Left a | Right b deriving Show


-- Either are equal if they are both Left and equal or both right and equal
instance {- ???  => -} Eq (Either a b) where
-- fill it in!

-- Left is less than Right, if the constructors are the same it matches the underlieing order of a and b
instance {- ???  => -} Ord (Either a b) where
-- fill it in!

-- note it could be a functor on both type args
-- keep it easy and just define it on 1
instance {- ???  => -} Functor (Either a ) where
-- fill it in!


-- a simple data used mostly for demonstration purposes

data Identity a = Identity a deriving Show

-- Identity is equal if the inner type is equal
instance {- ???  => -} Eq (Identity a) where
-- fill it in!

-- Identity has the same ordering as the inner type
instance {- ???  => -} Ord (Identity a) where
-- fill it in!

instance {- ???  => -} Functor Identity where
-- fill it in!


-- a simple data used mostly for demonstration purposes

data Trival a = NoA deriving Show

instance {- ???  => -} Eq (Trival a) where
-- fill it in!

instance {- ???  => -} Ord (Trival a) where
-- fill it in!

instance {- ???  => -} Functor Trival where
-- fill it in!
