module UsingLambdaCalcTest  where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty )

import UsingLambdaCalc(true, false, not, and, or, xor, zero, one, two, three, seven, add, mult, isEven)

toStandardBool b = b True False

toStandardInteger n = n (+1) 0

fromStandardInteger :: Integer -> (a -> a) -> a -> a
fromStandardInteger 0 f s = s
fromStandardInteger n f s = f (fromStandardInteger (n-1) f s )

basicTrueFalseTest = 
  do
    assertEqual "true returns first" ("then") (true "then" "else")
    assertEqual "false returns 2nd" ("else") (false "then" "else")

notTest = 
  do 
    assertEqual "(not true) -> false" False $ toStandardBool (UsingLambdaCalc.not true)
    assertEqual "(not false) -> true" True $ toStandardBool (UsingLambdaCalc.not false)
    
andTest = 
  do 
    assertEqual "(and true true) -> true"    True $ toStandardBool (UsingLambdaCalc.and true true)
    assertEqual "(and true false) -> false"  False $ toStandardBool (UsingLambdaCalc.and true false)
    assertEqual "(and false true) -> false"  False $ toStandardBool (UsingLambdaCalc.and false true)
    assertEqual "(and false false) -> false" False $ toStandardBool (UsingLambdaCalc.and false false)

orTest = 
  do 
    assertEqual "(or true true) -> true"    True $ toStandardBool (UsingLambdaCalc.or true true)
    assertEqual "(or true false) -> true"   True $ toStandardBool (UsingLambdaCalc.or true false)
    assertEqual "(or false true) -> true"   True $ toStandardBool (UsingLambdaCalc.or false true)
    assertEqual "(or false false) -> false" False $ toStandardBool (UsingLambdaCalc.or false false)

xorTest = 
  do 
    assertEqual "(xor true true) -> false"   False $ toStandardBool (UsingLambdaCalc.xor true true)
    assertEqual "(xor true false) -> true"   True $ toStandardBool (UsingLambdaCalc.xor true false)
    assertEqual "(xor false true) -> true"   True $ toStandardBool (UsingLambdaCalc.xor false true)
    assertEqual "(xor false false) -> false" False $ toStandardBool (UsingLambdaCalc.xor false false)
               

basicNumberTest = 
  do 
    assertEqual "0 ok" 0 $ toStandardInteger zero
    assertEqual "1 ok" 1 $ toStandardInteger one
    assertEqual "2 ok" 2 $ toStandardInteger two
    assertEqual "3 ok" 3 $ toStandardInteger three
    assertEqual "7 ok" 7 $ toStandardInteger seven

usingLambdaCalcTest = testGroup "UsingLambdaCalc test" [
    testCase "basic true false definition"  basicTrueFalseTest,
    testCase "not test" notTest,
    testCase "and test" andTest,
    testCase "or test" orTest,
    testCase "xor test" xorTest,

    testCase "basic number test" basicNumberTest,

    testProperty "add is correct" $ (((\n m -> if m >= 0 && n >= 0
                                                  then  (n + m == (toStandardInteger $ add (fromStandardInteger n) (fromStandardInteger m)))
                                                  else True)):: (Integer -> Integer -> Bool)),

    testProperty "mult is correct" $ (((\n m -> if m >= 0 && n >= 0
                                                  then  (n * m == (toStandardInteger $ mult (fromStandardInteger n) (fromStandardInteger m)))
                                                  else True)):: (Integer -> Integer -> Bool)) ,

    testProperty "isEven is correct" $ (((\n -> if n >= 0
                                                  then  ((mod n 2) == 0) == (toStandardBool $ isEven $ fromStandardInteger n)--isEven (fromStandardInteger n)))
                                                  else True)):: ( Integer -> Bool))                                 
  ]



  