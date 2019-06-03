{-# LANGUAGE StandaloneDeriving #-}
module Lang4Test where


import Lang4 (Ast(..), eval, Env)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import qualified Data.Map as Map


import Test.Tasty.QuickCheck

vars = ["x","y","z"]

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = 
  do i <- arbitrary
     v <- elements vars
     elements [LiteralInt i, Var v]                      
arbitrarySizedAst m | otherwise =
  do l <- arbitrarySizedAst (m `div` 2)
     r <- arbitrarySizedAst (m `div` 2)
     i <- arbitrary
     v <- elements vars
     elements [LiteralInt i, Var v, Plus l r, Mult l r, Let v l r]
     

deriving instance Eq Ast 

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst
    shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
    shrink (Var _) = []
    shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
    shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]
    shrink (Let v x y) = [y,x] ++ [Let v x' y' | (x', y') <- shrink (x, y)]



--TODO: better messages
tests = testGroup "Test lang 4" [

    testProperty "for all integer i, evaluation of LiteralInt i should be i." $
            \env i -> eval (LiteralInt i) env == Just i,
    testProperty "for all Vars v, evaluation should look up from the environment" $
        \env v i ->  
            let
                env' = Map.insert v i env
                mi = eval (Var v) env'
             in Just i == mi ,  
			 
    testProperty "For all ast1 & ast2, `Plus ast1 ast2` should evaluate to `(eval ast1) + (eval ast2)`" $
            \env ast1 ast2 -> eval (Plus ast1 ast2) env == do a1 <- (eval ast1 env); a2 <-(eval ast2 env); pure $ a1 + a2,
			
    testProperty "For all ast1 & ast2, `Mult ast1 ast2` should evaluate to `(eval ast1) * (eval ast2)`" $
            \env ast1 ast2 -> eval (Mult ast1 ast2) env == do a1 <- (eval ast1 env); a2 <-(eval ast2 env); pure $ a1 * a2,
			
    testProperty "For all Let, it should evaluate the expression in the appropriate environment" $
        \env v withThis inThis ->  
            case  eval withThis env of (Just i) -> (eval inThis $ Map.insert v i env) == eval (Let v withThis inThis) env
                                       Nothing -> True -- TODO: this is bad 			
  ]
