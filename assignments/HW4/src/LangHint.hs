-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework

module LangHint where

import Lang (Env, Ast(..), Val(..), run, showPretty)
import EnvUnsafe


-- some helper function, you may find helpful
valOf :: String -> EnvUnsafe Env Val
valOf var = undefined

-- add a val into the environment
withVal :: String -> Val -> EnvUnsafe Env a -> EnvUnsafe Env a
withVal var v comp = undefined

-- helper functions that take care of type issues (use a "Error" when things have the wron type
evalInt :: Ast -> EnvUnsafe Env Integer
evalInt a = undefined

evalBool :: Ast -> EnvUnsafe Env Bool
evalBool a = undefined

evalFun :: Ast -> EnvUnsafe Env (Val -> Unsafe Val)
evalFun a = undefined
-- ...

-- ungraded bonus challenge: use a helper type class to do this functionality


--some examples:

e1 = showPretty (Minus (ValInt 100) (Minus (ValInt 2) (ValInt 5))) $  0
e2 = showPretty (Minus (Minus (ValInt 100) (ValInt 2)) (ValInt 5) ) $  0

e3 = showPretty (Minus (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
e4 = showPretty (Div (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
e5 = showPretty (((Var "fun") `App` (ValInt 2)) `App` (ValInt 5)) $ 0

e6 = showPretty (Not $ Not $ ((Var "fun") `App` (ValInt 2)) `App` (Not $ ValInt 5)) $ 0

example = let x = Var "x"
          in App (Lam "x" ( x `Plus` x))  (ValInt 7)
example' = run example
		  
example2 = let x = Var "x"; y = Var "y"
           in ((Lam "x" (Lam "y" ( x `Plus` y))) `App` (ValInt 7)) `App` (ValInt 4)
example2' = run example2

example3 = let x = Var "x"; y = Var "y"
           in Lam "x" (Lam "y" ( x `Plus` y))
example3' = run example3

example4 = let x = Var "x"; y = Var "y"
           in (Cons $ Lam "x" (Lam "y" ( x `Plus` y))) $  (Cons ( Lam "x" (Lam "y" ( x `Minus` y))) $ Nil)
example4' = run example4