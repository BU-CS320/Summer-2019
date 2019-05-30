-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module Lang4Hint where

import Data.Map (Map)-- for env
import qualified Data.Map as Map

import Reader
import Lang4(Ast(..),Env)

-- unlike Lang3 there is no way to set a Var!

-- but a getVar function is still helpful
getVar :: String -> Reader Env Integer
getVar v = undefined -- use the "ask" function, or the constructor 


-- so your Let case would look like

eval :: Ast -> Reader Env Integer
-- ...
eval (Let v val bod) = 
  do val' <- eval val
     local (Map.insert undefined undefined) (eval undefined)

-- you can also use the Reader constructor directly
eval (Let v val bod) = 
  do val' <- eval val
     Reader (\env -> runReader (eval undefined) (Map.insert undefined undefined env))



-- some examples
ex = Let "x" (LiteralInt 2 `Plus` LiteralInt 2) (Var "x" `Mult` LiteralInt 5 )
       
-- run your monad like this
ex' = runReader (eval ex) Map.empty

ex1 = Let "x" (LiteralInt 10) (Let "y" (LiteralInt 2 `Plus` Var "x") (Var "x" `Mult` Var "y" ))
       
ex1' = runReader (eval ex1) Map.empty

e :: Env
e = Map.fromList([("w",10)])

ex2 = Let "x" (ex1 `Plus` ex) (Var "x" `Mult` (Let "z" (Var "x" `Sub` LiteralInt 1) (Var "z" `Sub` Var "w")))
       
ex2' = runReader (eval ex2) e

{-

Lang4> ex
Let "x" (Plus (LiteralInt 2) (LiteralInt 2)) (Mult (Var "x") (LiteralInt 5))

Lang4> ex'
20

Lang4> ex1
Let "x" (LiteralInt 10) (Let "y" (Plus (LiteralInt 2) (Var "x")) (Mult (Var "x") (Var "y")))

Lang4> ex1'
120

Lang4> ex2
Let "x" (Plus (Let "x" (LiteralInt 10) (Let "y" (Plus (LiteralInt 2) (Var "x")) (Mult (Var "x") (Var "y")))) (Let "x" (Plus (LiteralInt 2) (LiteralInt 2)) (Mult (Var "x") (LiteralInt 5)))) (Mult (Var "x") (Let "z" (Sub (Var "x") (LiteralInt 1)) (Sub (Var "z") (Var "w"))))

Lang4> ex2'
18060

-}
