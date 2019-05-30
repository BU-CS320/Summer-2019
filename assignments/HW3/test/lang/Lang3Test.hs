-- TODO: redo this more like https://github.com/BU-CS320/Summer-2019/blob/master/assignments/HW2/tests/Lang3Test.hs

module Lang3Test where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck as QC
import Data.Map (Map)
import qualified Data.Map as Map

import Lang3 (Ast(..), eval, showFullyParen, showPretty, AssignmetState)
import Lang3TestTypes (VarName,varNameToString)
import State (runState,get,put)

evalOneExpGetState :: AssignmetState -> Ast -> AssignmetState
evalOneExpGetState state exp1 = 
    let
        (res1, state1) = runState (eval exp1) state
        in state1

evalTwoExpGetState :: AssignmetState -> Ast -> Ast -> AssignmetState
evalTwoExpGetState initState exp1 exp2 =
    let
        (res1, state1) = runState (eval exp1) initState
        (res2, state2) = runState (eval exp2) state1
        in state2

evalOneExpGetRes :: AssignmetState -> Ast -> Integer
evalOneExpGetRes s ast = 
    let 
        (res1, state1) = runState (eval ast) s
        in res1

evalTwoExpGetRes :: AssignmetState -> Ast -> Ast -> (Integer, Integer)
evalTwoExpGetRes initState exp1 exp2 =
    let
        (res1, state1) = runState (eval exp1) initState
        (res2, state2) = runState (eval exp2) state1
        in (res1, res2)

evalOneExpGetAll :: AssignmetState -> Ast -> (Integer, AssignmetState)
evalOneExpGetAll s ast =
    let
        (res1, state1) = runState (eval ast) s
        in (res1, state1)       

lang3Test = testGroup "Lang3 test" [
    stateTest,
    resTest
  ]

stateTest = testGroup "State test" [
    testProperty "For all initial state, ast1 and ast2, state of `Plus ast1 ast2` should be the same as `(eval ast1)` then `(eval ast2)`" $
        \s i j -> (evalTwoExpGetState s i j) == (evalOneExpGetState s (Plus i j)),

    testProperty "For all initial state, ast1 and ast2, state of `Sub ast1 ast2` should be the same as `(eval ast1)` then `(eval ast2)`" $
        \s i j -> (evalTwoExpGetState s i j) == (evalOneExpGetState s (Sub i j)),
    
    testProperty "For all initial state, ast1 and ast2, state of `Mult ast1 ast2` should be the same as `(eval ast1)` then `(eval ast2)`" $
        \s i j -> (evalTwoExpGetState s i j) == (evalOneExpGetState s (Mult i j)),

    testProperty "For all initial state, ast1 and ast2, state of `Separator ast1 ast2` should be the same as `(eval ast1)` then `(eval ast2)`" $
        \s i j -> (evalTwoExpGetState s i j) == (evalOneExpGetState s (Separator i j)),

    testProperty "For all variable name s, initial state and ast, `Assign s ast` should add {s: eval ast} to Map" $
        \vname s ast ->  
            let 
                vnameStr = varNameToString (vname::VarName)
                (r1,s1) = evalOneExpGetAll s ast
                s2 = (Map.insert vnameStr r1 s1)
                in evalOneExpGetState s (Assign vnameStr ast) == s2
  ]

resTest = testGroup "Result test" [
    testProperty "For all integer i, evaluate LiteralInt i should be evaluate to i" $
        \s i -> i == (evalOneExpGetRes s (LiteralInt i)),
    
    testProperty "For all initial state and str, `Var str` should lookup the Map" $
        \state name ->  
            let
                str = varNameToString (name::VarName)
                i = (Map.findWithDefault 0 str state)
                in i == evalOneExpGetRes state (Var str),  
    
    testProperty "For all initial state, ast1 and ast2, `Plus ast1 ast2` should be evaluate to `(eval ast1) + (eval ast2)`" $
        \s i j -> (evalOneExpGetRes s (Plus i j)) == 
            let
                (a,b) = evalTwoExpGetRes s i j
                in a+b,

    testProperty "For all initial state, ast1 and ast2, `Sub ast1 ast2` should be evaluate to `(eval ast1) - (eval ast2)`" $
        \s i j -> (evalOneExpGetRes s (Sub i j)) == 
            let
                (a,b) = evalTwoExpGetRes s i j
                in a-b,
    testProperty "For all initial state, ast1 and ast2, `Mult ast1 ast2` should be evaluate to `(eval ast1) * (eval ast2)`" $
        \s i j -> (evalOneExpGetRes s (Mult i j)) == 
            let
                (a,b) = evalTwoExpGetRes s i j
                in a*b,
    testProperty "For all initial state, ast1 and ast2, `Separator ast1 ast2` should be evaluate to `(eval ast2)`" $
        \s i j -> (evalOneExpGetRes s (Separator i j)) == 
            let
                (a,b) = evalTwoExpGetRes s i j
                in b,

    testProperty "For all initial state and ast, `Assign s ast` should be the same as eval ast" $
        \vname state ast ->  
            let 
                str = varNameToString (vname::VarName)
                r1 = evalOneExpGetRes state (Assign str ast)
                r2 = evalOneExpGetRes state ast
                in r2 == r1  
  ]


