module Lang3 where

import Prelude hiding (lookup, insert) -- save us a good function name

import Data.Map (Map)-- for state
import qualified Data.Map as Map

import HelpShow

import State


data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Assign String Ast
    | Separator Ast Ast
    | Sub Ast Ast
    | Mult Ast Ast
  deriving Show

type AssignmetState = Map String Integer


-- for simplicity you will not need to separately encode failure at the type level,
-- Instead, you may return 0 for variables that are not defined

eval :: Ast -> State AssignmetState Integer
eval = undefined



-- show the fully parenthesized syntax
showFullyParen :: Ast -> String
showFullyParen (LiteralInt i)    = show i
showFullyParen (Var s)           = s
showFullyParen (l `Plus` r)      = "(" ++ showFullyParen l ++ " + " ++  showFullyParen r ++ ")"
showFullyParen (l `Sub` r)       = "(" ++ showFullyParen l ++ " - " ++  showFullyParen r ++ ")"
showFullyParen (l `Mult` r)      = "(" ++ showFullyParen l ++ " * " ++  showFullyParen r ++ ")"
showFullyParen (l `Separator` r) = "(" ++ showFullyParen l ++ " ; " ++  showFullyParen r ++ ")"
showFullyParen (Assign v b)      = "(" ++ v ++ " := " ++ show b ++ ")"


showPretty :: Ast -> Integer -> String
showPretty (LiteralInt i)     _ = show i
showPretty (Var s)            _ = s
showPretty (l `Mult` r)       d = parenthesize d 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0))
showPretty (l `Plus` r)       d = parenthesize d 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
showPretty (l `Sub` r)        d = parenthesize d 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2))
showPretty (Assign v b)       d = parenthesize d 6 (v ++ " := " ++  (showPretty b 6) )
showPretty (l `Separator` r)  d = parenthesize d 8 ((showPretty l 8) ++ " ; " ++  (showPretty r 7) ) -- binds most weakly


--instance Show Ast where
--  show e = showPretty e 100
