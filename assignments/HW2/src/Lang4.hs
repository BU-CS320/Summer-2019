module Lang4 where

-- we will use the standard Map
import Data.Map (Map)-- for Env
import qualified Data.Map as Map

-- We will now add let expressions to the abstract syntax tree. 
-- Let String Ast Ast makes a local assignment of the first Ast to the String
-- in the state and evaluates the second Ast in this environment and returns
-- the result of the second Ast. 

data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Mult Ast Ast
    | Let String Ast Ast

type Env = Map String Integer

eval :: Ast -> Env -> Maybe Integer
eval = undefined


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (Var s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
  show (Let s val inThis) =  "(let " ++ s ++ " = " ++ show val ++ " in " ++ show inThis ++ ")"
