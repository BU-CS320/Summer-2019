module Lang2 where

import PrinterMonad

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Separator Ast Ast
  | Print Ast
  | Sub Ast Ast
  | Mult Ast Ast


eval :: Ast -> PrinterMonad Integer Integer
eval (LiteralInt i) = return i -- PrinterMonad [] i
eval (Print x) =
  do x' <- eval x
     -- PrinterMonad [x'] x'
     printIt x'
     return x'
eval (Plus l r) =
  do l' <- eval l
     r' <- eval r
     return (l' + r')

printIt :: a -> PrinterMonad a ()
printIt a = PrinterMonad [a] ()
	 
ex = Print (LiteralInt 2 `Plus` LiteralInt 5)
       `Plus` Print (LiteralInt 1 `Plus` LiteralInt 1)
       
	   
ex' = runPrinterMonad (eval ex)
	   
	   
-- Ungraded bonus: There is a built in monad on tuples with the same functionality
eval' :: Ast -> ([Integer], Integer)
eval' =  undefined

-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Print b) =  "print(" ++ show b ++ ")"
  show (l `Sub` r) = "(" ++ (show l) ++ " - " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
