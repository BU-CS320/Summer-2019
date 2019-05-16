module Lang4Test where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck as QC

import Lang4 (Ast(..), eval, showFullyParen)
import Lang4TestTypes
import Reader

import HelpShow


import Data.Map (Map,empty,insert)-- for Env
type Env = Map String Integer

evalOneExpGetRes :: Env -> Ast -> Integer
evalOneExpGetRes initState exp1 = runReader (eval exp1) initState


showPretty :: Ast -> Integer -> String
showPretty (LiteralInt i)     _ = show i
showPretty (Var s)            _ = s
showPretty (l `Mult` r)       d = parenthesize d 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0))
showPretty (l `Plus` r)       d = parenthesize d 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
showPretty (l `Sub` r)        d = parenthesize d 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2))
showPretty (Let s val inThis) d = parenthesize d 5  ("let " ++ s ++ " = " ++ showPretty val 4 ++ " in " ++ showPretty inThis 5 ) -- binds most weakly



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
