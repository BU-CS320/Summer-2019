{-#LANGUAGE GADTs, ScopedTypeVariables #-}
-- I am sorry, there is just no way to write haskell code without these two

module LambdaTestTypes where
  
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck

  import HelpShow (parenthesize)
  import LambdaCalcImplementation (Term(..), mkFreeVar, mkApp, bindToLam)

  -- | help to generate random term
  -- these are valid var name when generating a random name
  -- this is to avoid empty or large variable name.
  validVarName = ["x", "y", "z", "test", "test2"]

  arbitrarySizedLambda ::  Int -> Gen Term
  arbitrarySizedLambda m | m < 1 = 
    do 
      str <- elements validVarName
      elements [mkFreeVar str]

  arbitrarySizedLambda m | otherwise = 
    do 
      l <- arbitrarySizedLambda (m `div` 2)
      r <- arbitrarySizedLambda (m `div` 2)
      str <- elements validVarName
      node <- elements [mkFreeVar str, mkApp l r, bindToLam str l]
      return node

  instance Arbitrary Term where
    arbitrary = sized arbitrarySizedLambda
