module Lang0 where

data Ast =
    LiteralInt Integer
  | Plus Ast Ast



eval :: Ast -> Integer
eval (LiteralInt i) = i
eval (x `Plus` y) = (eval x) + (eval y)


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
