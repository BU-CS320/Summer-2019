-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework

module CaptureAvoiding where
import Data.Set (Set)
import qualified Data.Set as Set

import HelpShow

-- this is the obvious way to do it

-- λ x . ( λ y . λ x . y  ) x
ex1 = Lam "x" $ (Lam "y" $  Lam "x" $ Var "x") `App` Var "x"

-- f a b c
-- ((f a) b) c
ex2 = Var "f" `App` Var "a" `App` Var "b" `App` Var "c"

-- f a (λ x . x) c
ex3 = Var "f" `App` Var "a" `App` (Lam "x" $ Var "x") `App` Var "c"


-- good:
--  easy to show/debug
--  very concrete
-- bad:
--  very hard to evaluate correctly (no-one has ever done it right the first time)
--  lots of additional information that needs to be ignored
-- variations:
--  use a monad to help with free var bookkeeping
--  store a list of applications

data Term = Var String
          | App Term Term
          | Lam String Term

mkFreeVar :: String -> Term
mkFreeVar = Var
mkApp  ::  Term -> Term -> Term
mkApp = App
bindToLam :: String -> Term -> Term
bindToLam = Lam

-- you will probably need the following helper functions
boundVars :: Term -> Set String
boundVars (Lam v bod) = Set.insert v $ boundVars bod
boundVars (Var _) = Set.empty
boundVars (App f a) = boundVars f `Set.union` boundVars a


-- rename a free var, in a term
rename :: Term -> String -> String -> Term
rename (App f a)      from to             = App (rename f from to) (rename a from to)
rename (Lam v bod)    from to | v == from = Lam v bod
rename (Lam v bod)    from to | otherwise = Lam v $ rename bod from to
rename (Var v)        from to | v == from = Var to
                              | otherwise = Var v

-- Apply a substitution to a term, being careful in any alpha-conversions to avoid a given set of variables
subst :: Term -> String -> Term -> Set String -> Term
subst (App f a)      from to avoid             = App (subst f from to avoid) (subst a from to avoid)
subst (Var v)        from to _ | v == from = to
                               | otherwise = Var v
subst (Lam v bod)    from to avoid | v == from = Lam v bod
                                   | otherwise =
  let v' = findName v avoid
      bod' = rename bod v v'
  in Lam v' $ subst bod' from to (Set.insert v' avoid)
                              
-- the last three functions are just to avoid the annoying case of
-- λ x . ( ( λ y . λ x . y ) ) x) 


show' :: Term -> Integer -> String 
show' (Var v) _ = v
show' (App f a) i = parenthesize i 1 (show' undefined 1 ++ " " ++ show' undefined 0)
show' (Lam v bod) i = parenthesize i 3  ("\\" ++ undefined ++ " -> " ++ show' undefined 3) 

showEx = App (Lam "x" (Var "x")) (Lam "y" (Var "y"))

instance Show Term where
  show t = show' t 100
  
instance Eq Term where
  _ == _ = undefined

