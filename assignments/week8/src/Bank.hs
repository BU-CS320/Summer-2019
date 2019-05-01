module Bank where 

-- optional practice problem

import Prelude hiding (lookup, insert) -- save us a good function name

import State

import Data.Map (Map)-- for state
import qualified Data.Map as Map

-- how much $ does each account have
type AcountState = Map String Integer 

makeAccount :: String -> State AcountState ()
makeAccount = undefined

-- starts at 0
closeAccount :: String -> State AcountState ()
closeAccount = undefined

-- only works on an open account
getBalence :: String -> State AcountState Integer
getBalence = undefined

-- only works on an open account, with non-negative money
deposit :: String -> Integer -> State AcountState ()
deposit = undefined

-- only works on an open account, 
-- can't withdraw more money than you don't have
withdraw :: String -> Integer -> State AcountState ()
withdraw = undefined


-- only works on open accounts
-- sender needs to have the money
transfer :: String -> String -> Integer -> State AcountState ()
transfer = undefined
