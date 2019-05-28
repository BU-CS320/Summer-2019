module Lang3 where

-- we will use the standard Map
import Data.Map (Map)-- for state
import qualified Data.Map as Map

-- We will now add identifiers and a global assignment command to the abstract syntax tree. 
-- Assignment should evaluate to the value of the assignment and store the value in the global memory state.
-- The state (containing values for variables) is passed along as the evaluation proceeds; as Assign
-- expressions are evaluated, bindings are added to the state, and when Var expressions are evaluated
-- they are looked up in the state.
-- where relevant, you should evaluate left to right. 

data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Mult Ast Ast
    | Assign String Ast
    | Separator Ast Ast


type State = Map String Integer

eval :: Ast -> State -> (Maybe Integer, State)
eval = undefined

-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (Var s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Assign s b) =  "(" ++ s ++ " := " ++ show b ++ ")"
