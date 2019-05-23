module TypeProblemsTest where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

import TypeProblems(Answer(..), q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q16,q17)
import Data.Bits
import Data.Char
import Data.Maybe

typeProblemTests =
  testGroup
    "TypeProblems"
    [definedTests,differentQ6Q7,differentQ8Q9, testCase "expect 13 answers" $ assertEqual [] 13 $ numAns]

-- for simple stuff
instance Eq a => Eq (Answer a) where
  Impossible == Impossible = True
  (Example x) == (Example y) = x == y
  _ == _ = False

checkDifferent :: (Answer a) -> (Answer a) -> (a -> a -> Bool) -> Bool
checkDifferent Impossible _ _ = True
checkDifferent _ Impossible _ = True
checkDifferent (Example x) (Example y) f = f x y

checkFullyDefined :: (Answer a) -> (a -> Bool) -> (Bool)
checkFullyDefined x ch =
  case (x) of
   (Example e) -> ch e
   (Impossible) -> True

eqSelf a = a == a


defineQ1test = testCase "Q1 is defined" $ assertBool [] $ checkFullyDefined q1 eqSelf
defineQ2test = testCase "Q2 is defined" $ assertBool [] $ checkFullyDefined q2 eqSelf
defineQ3test = testCase "Q3 is defined" $ assertBool [] $ checkFullyDefined q3 ( \ a -> eqSelf $ a 1 True 'a' 7 )
defineQ4test = testCase "Q4 is defined" $ assertBool [] $ checkFullyDefined q4 ( \ a -> eqSelf $ a 7 )
defineQ5test = testCase "Q5 is defined" $ assertBool [] $ checkFullyDefined q5 ( \ a -> eqSelf $ ((a 7) :: Bool))
defineQ6test = testCase "Q6 is defined" $ assertBool [] $ checkFullyDefined q6 ( \ a -> eqSelf $ a 7)
defineQ7test = testCase "Q7 is defined" $ assertBool [] $ checkFullyDefined q7 ( \ a -> eqSelf $ a 7)
defineQ8test = testCase "Q8 is defined" $ assertBool [] $ checkFullyDefined q8 ( \ a -> eqSelf $ a 7 8)
defineQ9test = testCase "Q9 is defined" $ assertBool [] $ checkFullyDefined q9 ( \ a -> eqSelf $ a 7 8)
defineQ10test = testCase "Q10 is defined" $ assertBool [] $ checkFullyDefined q10 ( \ a -> eqSelf $ a 7 True )
defineQ11test = testCase "Q11 is defined" $ assertBool [] $ checkFullyDefined q11 ( \ a -> eqSelf $ a 7 True )
defineQ12test = testCase "Q12 is defined" $ assertBool [] $ checkFullyDefined q12 ( \ a -> eqSelf $ a (\_ -> 'a') )
defineQ13test = testCase "Q13 is defined" $ assertBool [] $ checkFullyDefined q13 ( \ a -> eqSelf $ a (\_ -> 'a') )
defineQ14test = testCase "Q14 is defined" $ assertBool [] $ checkFullyDefined q14 ( \ a -> eqSelf $ a 7 'a')
defineQ15test = testCase "Q15 is defined" $ assertBool [] $ checkFullyDefined q15 ( \ a -> eqSelf $ a (\_ -> 'a') (\_ -> 'a') 'a')
defineQ16test = testCase "Q16 is defined" $ assertBool [] $ checkFullyDefined q16 ( \ a -> eqSelf $ ( a (\_ -> 'a') (\_ -> 'a') 'a' :: Char))
defineQ17test = testCase "Q17 is defined" $ assertBool [] $ checkFullyDefined q17 ( \ a -> eqSelf $ a (\_ -> 'a') (\_ -> 'a') )


--Make sure they didn't just write undefined
definedTests =
    testGroup
        "check TypeProblems defined"
    [
    defineQ1test,
    defineQ2test,
    defineQ3test,
    defineQ4test,
    defineQ5test,
    defineQ6test,
    defineQ7test,
    defineQ8test,
    defineQ9test,
    defineQ10test,
    defineQ11test,
    defineQ12test,
    defineQ13test,
    defineQ14test,
    defineQ15test,
    defineQ16test,
    defineQ17test
	
    ]

	

differentQ6Q7 =
    testCase "q6 different than q7 (different functions take one same input to different outputs)" $ assertBool [] $ checkDifferent q6 q7 (\a1 a2 -> a1 () /= a2 ())
differentQ8Q9 =
    testCase "q8 different than q8 (different functions take one same input to different outputs)" $ assertBool [] $ checkDifferent q8 q9 (\a1 a2 -> a1 True False /= a2 True False )
	
numAns = length $ filter (\x -> x) $ [isExample q1, isExample q2, isExample q3, isExample q4, isExample q5, isExample q6, isExample q7, isExample q8, isExample q9, isExample q10, isExample q11, isExample q12, isExample q13, isExample q14, isExample q15, isExample q16, isExample q17]

isExample (Example _) = True
isExample _ = False