module LambdaCalcImplementation where

import Data.Set (Set)
import qualified Data.Set as Set

data Term -- = ...

-- 3 helper functions for the graders

-- makes a free var from a string
mkFreeVar :: String -> Term
mkFreeVar = undefined

-- applies the 2nd term to the first
mkApp  ::  Term -> Term -> Term
mkApp = undefined

-- creates a lambda out of a term and the name of a free var
bindToLam :: String -> Term -> Term
bindToLam = undefined


-- true if there no evaluation that can be done in the lambda expression
isValue :: Term -> Bool
isValue = undefined

-- collect all the vars that appear in the expression that are not bound
freeVars :: Term -> Set String
freeVars = undefined

-- when are there no free variables in a lambda expression?
isClosed :: Term -> Bool
isClosed = undefined


-- do all possible applications, rename bound variables as needed
eval :: Term -> Term
eval = undefined



-- show the unevaluated expression, in a human readable way.  It must be compatible with your parser.
-- you may choose a to show the fully parenthesized version
instance Show Term where
  show _ = undefined

-- equality on the structure of the term, where the names of bindings don't matter.
-- this is called alpha-equality
instance Eq Term where
  _ == _ = undefined

