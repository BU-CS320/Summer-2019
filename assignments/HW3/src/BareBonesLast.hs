module BareBonesLast where


import Prelude hiding (List(..), Pair(..),Maybe(..),Either(..),)

import Control.Monad(ap) -- for the things we will ignore right now

-- Implement the following type class instances over our bare bones data
-- You may add any type class dependencies you see fit
-- Follow the instructions, your implementations must obey the standard type class laws.

-- Note: sometimes laws and typing rules completely define the implementation, 
-- if you don't see instructions look up the laws.


data List a = Nil | Cons a (List a) deriving Show


-- lists are equal when they have equal members in the same order
instance {- ???  => -} Eq (List a) where
  _ == _ = undefined

-- lists are ordered lexicographically over elements
instance {- ???  => -} Ord (List a) where
  compare = undefined -- or use <=

instance {- ???  => -} Functor List where
  fmap = undefined


-- Ignore this for now
instance Applicative List where
  pure = return
  (<*>) = ap


instance Monad List where
  -- returns a singleton list containing x
-- Prelude> (return True) :: [Bool]
-- [True]
-- Prelude> (return 7) :: [Integer]
-- [7]
  return x = undefined
  
  -- apply function to every value in the list, and flatten the results (maintaining the order).
  --  [1,2,3] >>= (\x -> [x+10,x+100])     ==    [11,101,12,102,13,103]
  _ >>= _ = undefined


  
data Maybe a = Nothing | Just a deriving Show


-- Maybe are equal if they are both Just the same thing, or both Nothing
instance {- ???  => -} Eq (Maybe a) where
  _ == _ = undefined

-- "Nothing" is less than Just, otherwise it matches the order of a
instance {- ???  => -} Ord (Maybe a) where
  compare = undefined -- or use <=

instance {- ???  => -} Functor Maybe where
  fmap = undefined


-- Ignore this for now
instance Applicative Maybe where
  pure = return
  (<*>) = ap


instance Monad Maybe where
  -- should be the same as Just 
  return x = Just x

  Nothing >>= f = Nothing
  Just x  >>= f = f x
-- f :: a -> Maybe b (bound at src\BareBonesLast.hs:48:14)
-- x :: a (bound at src\BareBonesLast.hs:48:8)
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b


  -- on Just, apply the function
-- Prelude> (Just 7) >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Just 9
-- Prelude> (Just 3) >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Nothing
-- Prelude> Nothing >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Nothing
-- Prelude> Nothing >>= (undefined) -- a little wierd
-- Nothing
  
data  Either a b  =  Left a | Right b deriving Show

-- Either are equal if they are both Left and equal or both right and equal
instance {- ???  => -} Eq (Either a b) where
  _ == _ = undefined

-- Left is less than Right, if the constructors are the same it matches the underlieing order of a and b
instance {- ???  => -} Ord (Either a b) where
  compare = undefined -- or use <=

-- note it could be a functor on both type args
-- keep it easy and just define it on 1
instance {- ???  => -} Functor (Either a ) where
  fmap = undefined

-- Ignore this for now
instance Applicative (Either a) where
  pure = return
  (<*>) = ap


instance Monad (Either a) where
  return x = undefined
  
  _ >>= _ = undefined

-- While Monad (Either a) is completely determined by the types and the laws 
-- here are some examples hints, note this is sometimes called the "Error Monad"

-- Prelude> (return 7) :: Either a Integer
-- Right 7
-- Prelude> (Right 3) >>= (\ x -> if x == 3 then (Left "whoops") else Right (x+2))
-- Left "whoops"
-- Prelude> (Right 7) >>= (\ x -> if x == 3 then (Left "whoops") else Right (x+2))
-- Right 9
-- Prelude> (Left "uhoh") >>= (\ x -> if x == 3 then (Left "whoops") else Right (x+2))
-- Left "uhoh"
  

-- a simple data used mostly for demonstration purposes

data Identity a = Identity a deriving Show

runIdentity (Identity a) = a

-- Identity is equal if the inner type is equal
instance {- ???  => -} Eq (Identity a) where
  _ == _ = undefined

-- Identity has the same ordering as the inner type
instance {- ???  => -} Ord (Identity a) where
  compare = undefined -- or use <=

instance {- ???  => -} Functor Identity where
  fmap = undefined


-- Ignore this for now
instance Applicative Identity where
  pure = return
  (<*>) = ap

  
instance Monad Identity where
  return x = undefined
  
  _ >>= _ = undefined


-- a simple data used mostly for demonstration purposes

data Trival a = NoA deriving Show
  

instance {- ???  => -} Eq (Trival a) where
  _ == _ = undefined

instance {- ???  => -} Ord (Trival a) where
  compare = undefined -- or use <=

instance {- ???  => -} Functor Trival where
-- fill it in!

-- Ignore this for now
instance Applicative Trival where
  pure = return
  (<*>) = ap

instance Monad Trival where
  return x = undefined
  
  _ >>= _ = undefined

  
  
data Pair a b = Pair a b deriving Show -- remember same as Pair a b = (a,b) in standard Haskell

-- Pairs are equal when they have equal members in the same order
instance {- ???  => -} Eq (Pair a b) where
  _ == _ = undefined

-- Pair are ordered lexicographically over elements
instance {- ???  => -} Ord (Pair a b) where
  compare = undefined -- or use <=

-- note it could be a functor on both type args
-- keep it easy and just define it on 1
instance {- ???  => -} Functor (Pair a) where
  fmap = undefined


-- ungraded bonus

-- Ignore this for now
instance Monoid a => Applicative (Pair a) where
  pure = return
  (<*>) = ap

-- note that the standard implementation is more interesting and is often called the Writer Monad
--    ([True],7) >>= (\ x -> ([False],x+2))       ==      ([True,False],9)
instance Monoid a => Monad (Pair a) where
  return x = undefined
  
  _ >>= _ = undefined
