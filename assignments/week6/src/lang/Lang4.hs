module Lang4 where

import Data.Map (Map)-- for Env
import qualified Data.Map as Map


data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Let String Ast Ast

type Env = Map String Integer

 -- ungraded bonus, helpful for testing
eval :: Ast -> Env -> Maybe Integer
eval = undefined


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (Var s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Let s val inThis) =  "(let " ++ s ++ " = " ++ show val ++ " in " ++ show inThis ++ ")"
