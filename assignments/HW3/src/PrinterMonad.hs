module PrinterMonad where

import Control.Monad(ap)

-- This is optional!

--This monad will form the plumbing for the Lang2 evaluation function


data PrinterMonad out a = PrinterMonad [out] a

-- function that just runs the function contained in PrinterMonad
runPrinterMonad::  (PrinterMonad out a) -> ([out], a)
runPrinterMonad (PrinterMonad ls a) = (ls, a)



instance Functor (PrinterMonad out) where
  -- fmap :: (a -> b) -> PrinterMonad out a -> PrinterMonad out b
  fmap f (PrinterMonad _ _) = undefined
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (PrinterMonad out) where
  pure = return
  (<*>) = ap

instance Monad (PrinterMonad out) where
  --return :: a -> PrinterMonad out a
  -- for this monad, return should not print anything!
  return a = undefined

  --(>>=) :: PrinterMonad out a -> (a -> PrinterMonad out b) -> PrinterMonad out b
  -- for this monad, append the 1st print buffer in front of the 2nd
  (PrinterMonad _ _) >>= f = undefined

  -- make sure your implementation follows the Monad laws

-- technical note: 
-- this is a special case of what is called the writer monad.
