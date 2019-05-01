module BareBonesLast where


import Prelude hiding (List(..), Pair(..),Maybe(..),Either(..),)

import Control.Monad(ap) -- for the things we will ignore right now

-- Implement the following type class instances over our bare bones data
-- You may add any dependencies you see fit
-- Follow the instructions, your implementations must obey the standerd type class laws.
-- type class laws will explained in lecture on W 2/30 and explored in the analytical part of this homework (if you get stuck here, look at that). 
-- Note that sometimes laws and typing rules completely define the implementation, 
-- if you don't see instructions look up the laws.


data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  fmap f _ = undefined  -- you should have done this in week 5

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


  
data Maybe a = Nothing | Just a deriving (Show, Eq)

instance Monad Maybe where
  -- should be the same as Just 
  return x = Just x

  Nothing >>= f = Nothing
  Just x  >>= f = f x
-- f :: a -> Maybe b (bound at src\BareBonesLast.hs:48:14)
-- x :: a (bound at src\BareBonesLast.hs:48:8)
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

instance Functor Maybe where
  fmap f _ = undefined  -- you should have done this in week 5

-- Ignore this for now
instance Applicative Maybe where
  pure = return
  (<*>) = ap


  -- on Just, apply the function
-- Prelude> (Just 7) >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Just 9
-- Prelude> (Just 3) >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Nothing
-- Prelude> Nothing >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Nothing
-- Prelude> Nothing >>= (undefined) -- a little wierd
-- Nothing
  
data  Either a b  =  Left a | Right b deriving (Show, Eq)

instance Functor (Either a) where
  fmap f _ = undefined  -- you should have done this in week 5

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

data Identity a = Identity a deriving (Show, Eq)

runIdentity (Identity a) = a

instance Functor Identity where
  fmap f _ = undefined  -- you should have done this in week 5

-- Ignore this for now
instance Applicative Identity where
  pure = return
  (<*>) = ap

  
instance Monad Identity where
  return x = undefined
  
  _ >>= _ = undefined


-- a simple data used mostly for demonstration purposes

data Trival a = NoA deriving (Show, Eq)
  

instance Functor Trival where
  fmap f _ = undefined  -- you should have done this in week 5

-- Ignore this for now
instance Applicative Trival where
  pure = return
  (<*>) = ap

instance Monad Trival where
  return x = undefined
  
  _ >>= _ = undefined

  
  
-- ungraded bonus

data Pair a b = Pair a b deriving (Show, Eq) -- remember same as Pair a b = (a,b) in standard Haskell

instance Functor (Pair a) where
  fmap f _ = undefined  -- you should have done this in week 5

-- Ignore this for now
instance Monoid a => Applicative (Pair a) where
  pure = return
  (<*>) = ap


-- note that the standard implementation is more interesting and is often called the Writer Monad
--    ([True],7) >>= (\ x -> ([False],x+2))       ==      ([True,False],9)
instance Monoid a => Monad (Pair a) where
  return x = undefined
  
  _ >>= _ = undefined
