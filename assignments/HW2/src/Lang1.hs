module Lang1 where

-- We have added division to the Ast.  Division in the language should act like Haskell's `div`.

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Mult Ast Ast
  | Div Ast Ast

-- Evaluate the AST to calculate its value
-- ex: eval (1 + (10 / 0)) + 2  ==> None
-- ex: eval (1 + (10 / 2)) + 2  ==> Just 8

eval :: Ast -> Maybe Integer
eval = undefined


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
