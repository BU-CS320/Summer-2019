-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module Lang3Hint where

import Data.Map (Map)-- for state
import qualified Data.Map as Map

import State
import Lang3(Ast(..),AssignmetState, eval)


-- You might want to use helper functions like
setVar :: String -> Integer -> State AssignmetState ()
setVar v i = 
  do s <- get
     put (Map.insert v i s)
     
-- same as
--State (\ s -> ((), Map.insert v i s))

getVar :: String -> State AssignmetState Integer
getVar v =
  do s <- get
     case (Map.lookup v s) of
       Just i  -> return i
       Nothing -> return 0  -- since for this problem we may return 0 if the var is not set
-- same as
--State (\ s -> ((case Map.lookup v s of
--                  Nothing -> 0 -- since for this problem we may return 0 if the var is not set
--                  Just i  -> i), s)

-- some examples
ex = Assign "x" (LiteralInt 2 `Plus` LiteralInt 2)
       `Separator` (Var "x" `Mult` LiteralInt 5 )
       
-- run your monad like this
ex' = runState (eval ex) Map.empty
