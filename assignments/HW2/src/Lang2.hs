module Lang2 where

-- We will make a very simple extension to the example AST by adding a separator this will 
-- join two abstract syntax trees; both will be evaluated, but only the value of the second
-- will be returned.

-- We will now add a print command to the abstract syntax tree. print takes one expression,
-- evaluates it, prints the result, and finally evaluates to that result.

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Separator Ast Ast
  | Print Ast

  
-- eval print(1); print(2)   ==> ([1,2], 2)
-- eval ((1 + print (10)) + 2 ; print(3)) ==> ([10,3], 3)
-- eval print(print(print(1) + 2) +3 ) +4  ==> ([1,3,6], 10)
-- eval print(print(1) + print(2)) ; 7   ==> ([1,2,3], 7)

eval :: Ast -> ([Integer], Integer)
eval = undefined


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Print b) =  "print(" ++ show b ++ ")"
