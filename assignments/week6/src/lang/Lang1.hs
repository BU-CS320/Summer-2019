module Lang1 where


data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Div Ast Ast

 -- ungraded bonus, helpful for testing
eval :: Ast -> Maybe Integer
eval = undefined

-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
