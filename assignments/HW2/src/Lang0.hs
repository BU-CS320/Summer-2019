module Lang0 where

-- Consider the language defined by the following AST (Abstract Syntax Tree) for adding numbers

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Mult Ast Ast

-- it can be evaluated with the function eval:
-- ex: eval ((1 + 3) + 2)  ==> 6

eval :: Ast -> Integer
eval = undefined


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
