-- This file is just for nints, examples, and test cases
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module ReaderHint where

import Prelude
import Data.Map (Map)-- for Env
import qualified Data.Map as Map
import Reader




type Env = Map String Integer

environment :: Env      
environment = Map.fromList([("x",4),("y",7)])

prog :: Reader Env Integer
prog = do e <- ask
          case Map.lookup "x" e of
             Nothing -> return 0
             Just n  -> return n

test :: String -> Reader Env Integer
test x  = do env <- ask
             return (Map.findWithDefault 0 x env)

ex1 = runReader prog environment

ex2 = runReader prog Map.empty

ex3 = runReader (test "y") environment

ex4 = runReader (test "z") environment

{-

Reader> ex1
4
Reader> ex2
0
Reader> ex3
7
Reader> ex4
0

-}


 
  
