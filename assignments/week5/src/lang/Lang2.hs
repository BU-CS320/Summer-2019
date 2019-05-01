module Lang2 where

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Separator Ast Ast
  | Print Ast

 -- ungraded bonus, helpful for testing
eval :: Ast -> ([Integer], Integer)
eval = undefined

-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Print b) =  "print(" ++ show b ++ ")"
