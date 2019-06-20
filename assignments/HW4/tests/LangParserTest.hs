module LangParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck


import ParserMonad (parse)

import Lang (showFullyParen, showPretty, Ast(..))
import LangParser (parser)


--TODO: move the generator to a shared place

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

    shrink (ValInt i) = [ValInt i' | i' <-  shrink i]
    shrink (ValBool i) = [ValBool i' | i' <-  shrink i]
    shrink (And l r) = [l, r] ++ [And l' r' | (l', r') <-  shrink (l, r)]
    shrink (Or l r) = [l, r] ++ [Or l' r' | (l', r') <-  shrink (l, r)]
    shrink (Not i) = [i] ++ [Not i' | i' <-  shrink i]
	
    shrink (Plus l r) = [l, r] ++ [Plus l' r' | (l', r') <-  shrink (l, r)]
    shrink (Minus l r) = [l, r] ++ [Minus l' r' | (l', r') <-  shrink (l, r)]
    shrink (Mult l r) = [l, r] ++ [Mult l' r' | (l', r') <-  shrink (l, r)]
    shrink (Div l r) = [l, r] ++ [Div l' r' | (l', r') <-  shrink (l, r)]
	
    shrink (If b t e) = [b, t, e] ++ [If b' t' e' | (b', t', e') <-  shrink (b, t, e)]
	
    shrink (Cons l r) = [l, r] ++ [Cons l' r' | (l', r') <-  shrink (l, r)]
	
    shrink (Let x l r) = [l, r] ++ [Let x l' r' | (l', r') <-  shrink (l, r)]
	
    shrink (Lam x l ) = [l] ++ [Lam x l'  | (l') <-  shrink (l)]
    shrink (App l r) = [l, r] ++ [App l' r' | (l', r') <-  shrink (l, r)]
	
    shrink _ = []
	

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- arbitrary
                                 b <- arbitrary
                                 node <- elements [ValInt i, ValBool b, Nil]
                                 return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
                                     str <- elements ["x","y","z"]
                                     ifast <- arbitrarySizedIf m
                                     node <- elements [And l r, Or l r, Not l,
                                                       Plus l r, Minus l r, Mult l r, Div l r,
                                                       Cons l r,
                                                       ifast,
                                                       Let str l r,
                                                       Lam str l,
                                                       App l r,
                                                       Var str
                                                      ]
                                     return node

-- it would be better if every branch were built like this so the balance would be maintained
arbitrarySizedIf ::  Int -> Gen Ast
arbitrarySizedIf m = do b <- arbitrarySizedAst (m `div` 3)
                        t <- arbitrarySizedAst (m `div` 3)
                        e <- arbitrarySizedAst (m `div` 3)
                        return $ If b t e

parserTest = testGroup
      "parser Test"
      [
      testProperty "parse should return the same AST when fully parenthesized" $ ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
      testProperty "parse should return the same AST when pretty printed" $ ((\ x -> Just (x , "") == (parse parser $ showPretty x 0)) :: Ast -> Bool)
      ]