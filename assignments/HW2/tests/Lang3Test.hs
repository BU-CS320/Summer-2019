{-# LANGUAGE StandaloneDeriving #-}
module Lang3Test where


import Lang3 (Ast(..), eval, State)

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
     elements [LiteralInt i, Var v, Plus l r, Mult l r, Separator l r, Assign v l]
     

deriving instance Eq Ast 

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst
    shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
    shrink (Var _) = []
    shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
    shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]
    shrink (Separator x y) = [x,y] ++ [Separator x' y' | (x', y') <- shrink (x, y)]
    shrink (Assign v x) = [x] ++ [Assign v x' | x' <- shrink x]


safeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
safeAdd (Just x) (Just y) = Just (x + y)
safeAdd _ _ = Nothing

safeDiv :: Maybe Integer -> Maybe Integer -> Maybe Integer
safeDiv (Just x) (Just 0) = Nothing
safeDiv (Just x) (Just y) = Just (x `div` y)
safeDiv _ _ = Nothing

safeMult :: Maybe Integer -> Maybe Integer -> Maybe Integer
safeMult (Just x) (Just y) = Just (x * y)
safeMult _ _ = Nothing


--TODO: better messages
--TODO: decouple the state and result tests
tests = testGroup "Test lang 3" [

  testGroup "test 'result'" [
      testProperty "For all integer i, evaluate LiteralInt i should be evaluate to i" $
        \s i -> Just i == fst (eval (LiteralInt i) s) ,
                    
    testProperty "For all initial state and str, `Var str` should lookup the Map" $
        \s v i ->  
            let
                s' = Map.insert v i s
                (mi,_) = eval (Var v) s'
             in Just i == mi ,  
    
      testProperty "For all initial state, ast1 and ast2, `Plus ast1 ast2` should be evaluate to `(eval ast1) + (eval ast2)`" $
        \s ast ast' ->
            let
                (l,s') = eval ast s 
                (r,s'') = eval ast' s' 
                in l `safeAdd` r == fst ( eval (ast `Plus` ast') s),

    testProperty "For all initial state, ast1 and ast2, `Mult ast1 ast2` should be evaluate to `(eval ast1) * (eval ast2)`" $
        \s ast ast' ->
            let
                (l,s') = eval ast s 
                (r,s'') = eval ast' s' 
            in l `safeMult` r ==  fst (eval (ast `Mult` ast') s),
                
    testProperty "For all initial state, ast1 and ast2, `Separator ast1 ast2` should be evaluate to `(eval ast2)`" $
        \s ast ast' ->
            let
                (l,s') = eval ast s 
                (r,s'') = eval ast' s' 
            in case l of (Just _) ->  r ==  fst (eval (ast `Separator` ast') s)
                         _ -> True, -- TODO: this is bad

    testProperty "For all initial state and ast, `Assign s ast` should be the same as eval ast" $
        \s ast v ->  
            let 
                (l,_) = eval (Assign v ast) s 
                (l',_) = eval ast s 
                in l == l'
  ],
  testGroup "state works correctly" [
    testProperty "looking up vars doesn't change state" $
        \s v -> s == snd (eval (Var v) s) ,
    
    testProperty "For all integer i, evaluate LiteralInt i should not change state" $
        \s i -> s == snd (eval (LiteralInt i) s) ,
        
    testProperty "(eval left to right) For all initial state, ast1 and ast2, state of `Plus ast1 ast2` should be the same as `(eval ast1)` then `(eval ast2)`" $
        \s ast ast' ->
            let
                (l,s') = eval ast s 
                (r,s'') = eval ast' s' 
                in case l of (Just _) -> s'' ==  snd ( eval (ast `Plus` ast') s )
                             _ -> True, -- TODO: this is bad
                
    
    testProperty "For all initial state, ast1 and ast2, state of `Mult ast1 ast2` should be the same as `(eval ast1)` then `(eval ast2)`" $
        \s ast ast' ->
            let
                (l,s') = eval ast s 
                (r,s'') = eval ast' s' 
                in case l of (Just _) -> s'' ==  snd ( eval (ast `Mult` ast') s )
                             _ -> True, -- TODO: this is bad

    testProperty "For all initial state, ast1 and ast2, state of `Separator ast1 ast2` should be the same as `(eval ast1)` then `(eval ast2)`" $
        \s ast ast' ->
            let
                (l,s') = eval ast s 
                (r,s'') = eval ast' s' 
                in case l of (Just _) -> s'' ==  snd ( eval (ast `Separator` ast') s )
                             _ -> True, -- TODO: this is bad

    testProperty "For all variable name s, initial state and ast, `Assign s ast` should add {s: eval ast} to Map" $
        \s v ast ->  
            let 
                (_,s') =  eval (Assign v ast) s
                in case eval (ast) s of (Just i,s1) -> s' == (Map.insert v i s1)
                                        _ -> True -- TODO: this is bad
    ]
  ]
