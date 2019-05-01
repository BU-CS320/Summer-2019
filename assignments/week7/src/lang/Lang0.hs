module Lang0 where

import BareBonesLast (Identity)

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Sub Ast Ast
  | Mult Ast Ast



eval :: Ast -> Integer
eval (LiteralInt i) = i
eval (x `Plus` y)   = (eval x) + (eval y)
eval (x `Sub` y)    = (eval x) - (eval y)
eval (x `Mult` y)   = (eval x) * (eval y)

-- the monadic way. silly here, but a strong hint for how to do the other languages.
eval' :: Ast -> Identity Integer
eval' (LiteralInt i) = return i
eval' (x `Plus` y) =
  do x' <- eval' x
     y' <- eval' y
     return (x' + y')
eval' (x `Sub` y) =
  do x' <- eval' x
     y' <- eval' y
     return (x' - y')
eval' (x `Mult` y) =
  do x' <- eval' x
     y' <- eval' y
     return (x' * y')

--  for all ast,  eval ast == runIdentity (eval' ast)
     
     
-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Sub` r) = "(" ++ (show l) ++ " - " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
