module Lang2 where

import HelpShow

import PrinterMonad

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Separator Ast Ast
  | Print Ast
  | Sub Ast Ast
  | Mult Ast Ast
  deriving Show


eval :: Ast -> PrinterMonad Integer Integer
eval =  undefined -- This is an optional ungraded part
  

showFullyParen :: Ast -> String
showFullyParen (LiteralInt i)    = show i
showFullyParen (l `Plus` r)      = "(" ++ showFullyParen l ++ " + " ++  showFullyParen r ++ ")"
showFullyParen (l `Sub` r)       = "(" ++ showFullyParen l ++ " - " ++  showFullyParen r ++ ")"
showFullyParen (l `Mult` r)      = "(" ++ showFullyParen l ++ " * " ++  showFullyParen r ++ ")"
showFullyParen (l `Separator` r) = "(" ++ showFullyParen l ++ " ; " ++  showFullyParen r ++ ")"
showFullyParen (Print b)         = "print(" ++ showFullyParen b ++ ")"


showPretty :: Ast  -- ^ the ast that we need to print
          -> Integer  -- ^ the precedence level of outter expression
          -> String  -- ^ the print result

showPretty (LiteralInt i)     _     = show i
showPretty (Print b)          _     = "print(" ++ showPretty b 100 ++ ")"  
showPretty (l `Mult` r)       outerLevel = parenthesize outerLevel 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0))
showPretty (l `Plus` r)       outerLevel = parenthesize outerLevel 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
showPretty (l `Sub` r)        outerLevel = parenthesize outerLevel 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2))
showPretty (l `Separator` r)  outerLevel = parenthesize outerLevel 8 ((showPretty l 8) ++ " ; " ++  (showPretty r 7)) 

--instance Show Ast where
--  show e = showPretty e 100
