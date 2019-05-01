module Lang3 where

import Data.Map (Map)-- for state
import qualified Data.Map as Map


data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Assign String Ast
    | Separator Ast Ast
    | Sub Ast Ast
    | Mult Ast Ast


type State = Map String Integer


 -- ungraded bonus. Hint: write a monad
--eval :: Ast -> ...
eval = undefined


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (Var s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Assign s b) =  "(" ++ s ++ " := " ++ show b ++ ")"
  show (l `Sub` r) = "(" ++ (show l) ++ " - " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
