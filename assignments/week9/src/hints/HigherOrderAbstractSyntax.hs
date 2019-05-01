-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework

module HigherOrderAbstractSyntax where

import Data.Set (Set)
import qualified Data.Set as Set

import HelpShow

-- use Haskell's internal notion of function to implement our language's function

-- λ x . ( λ y . λ x . y  ) x
ex1 = Lam $ \ x -> (Lam $ \ y ->  Lam $ \ x ->  y) `App` x

-- f a b c
-- ((f a) b) c
ex2 = FreeVar "f" `App` FreeVar "a" `App` FreeVar "b" `App` FreeVar "c"

-- f a (λ x . x) c
ex3 = FreeVar "f" `App` FreeVar "a" `App` ( Lam $ \ x -> x) `App` FreeVar "c"


-- good:
--  substitution
--  evaluation
--  easy to build up terms in haskell's syntax
-- bad:
--  lose var names
--  hard to do any introspection, show/debug, Eq, freeVars, isValue
--  can require more cleverness to work with
--  can put complex logic into the function that does not necessarily correspond to the Language.  This is less of an issue in this case since haskell is pure and turring complete, just like lambda calc
-- variations:
--  use polymorphism for FreeVar


data Term = FreeVar String
          | App Term Term
          | Lam (Term -> Term)



-- some helper functions for the graders
mkFreeVar :: String -> Term
mkFreeVar = FreeVar

mkApp  ::  Term -> Term -> Term
mkApp = App

subst ::  Term -> String -> Term -> Term
subst (FreeVar v') v withThis | v == v'   = withThis
subst (FreeVar v') v withThis | otherwise = (FreeVar v')
subst (App f a) v withThis                = App (subst f v withThis) (subst a  v withThis)
subst (Lam bod) v withThis                = Lam $ \x -> subst (bod x) v withThis

bindToLam :: String -> Term -> Term
bindToLam v t = Lam $ \x -> subst t v x






show' :: Term -> Integer -> NewNames String
show' (FreeVar v) _ = return v
show' (App f a) i = 
  do f' <- show' f 1
     a' <- show' a 0
     return $ parenthesize i 1 (undefined ++ " " ++ undefined) 
show' (Lam bod) i = 
  do v <- freshName
     bod' <- show' (bod undefined) 3
     return $  parenthesize i 3  ("\\" ++ undefined ++ " -> " ++ bod')

showEx = App (Lam $ \ x ->  x) (Lam $ \ y -> y)

instance Show Term where
  show term = fst $ runNewNames (show' term 100) (freeVars term) 0
  
 
-- pretty annoying
instance Eq Term where
  _ == _ = undefined


  
freeVars = undefined
