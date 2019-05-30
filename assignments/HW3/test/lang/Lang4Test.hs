
-- TODO: redo this more like https://github.com/BU-CS320/Summer-2019/blob/master/assignments/HW2/tests/Lang4Test.hs
module Lang4Test where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck as QC

import Lang4 (Ast(..), eval, showFullyParen, showPretty)
import Lang4TestTypes
import Reader



import Data.Map (Map,empty,insert)-- for Env
type Env = Map String Integer

evalOneExpGetRes :: Env -> Ast -> Integer
evalOneExpGetRes initState exp1 = runReader (eval exp1) initState


lang4Test = testGroup "Lang4 test" [
    baseCase,
    plusTest,
    subTest,
    multTest,
    letTest
    ]

baseCase = testProperty "Base case: for all integer i, evaluation of LiteralInt i should be i." $
            \i ranEnv -> runReader (eval (LiteralInt (i::Integer))) ranEnv == i

plusTest = testProperty "For all ast1 & ast2, `Plus ast1 ast2` should evaluate to `(eval ast1) + (eval ast2)`" $
            \ast1 ast2 ranEnv -> runReader (eval (Plus ast1 ast2)) ranEnv == 
                            runReader (eval ast1) ranEnv + runReader (eval ast2) ranEnv

subTest = testProperty "For all ast1 & ast2, `Sub ast1 ast2` should evaluate to `(eval ast1) - (eval ast2)`" $
            \ast1 ast2 ranEnv-> runReader (eval (Sub ast1 ast2)) ranEnv == 
                            runReader (eval ast1) ranEnv - runReader (eval ast2) ranEnv

multTest = testProperty "For all ast1 & ast2, `Mult ast1 ast2` should evaluate to `(eval ast1) * (eval ast2)`" $
            \ast1 ast2 ranEnv -> runReader (eval (Mult ast1 ast2)) ranEnv == 
                            runReader (eval ast1) ranEnv * runReader (eval ast2) ranEnv

letTest = testProperty "For all Let, it should evaluate the expression in the appropriate environment" $
            \astVal astBody ranVar ranEnv ->   
                let 
                    varName = (varNameToString (ranVar::VarName))
                    in
                        runReader (eval (Let varName astVal astBody)) 
                                  (ranEnv) ==
                        runReader (eval astBody) 
                                  (Data.Map.insert varName (runReader (eval astVal) (ranEnv)) ranEnv)
