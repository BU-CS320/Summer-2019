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
    [definedTests,differentQ6Q7,differentQ8Q9]

-- for simple stuff
instance Eq a => Eq (Answer a) where
  Impossible == Impossible = True
  (Example x) == (Example y) = x == y
  _ == _ = False

checkDifferent :: (Answer a) -> (Answer a) -> (a -> a -> Bool) -> Bool
checkDifferent Impossible _ _ = True
checkDifferent _ Impossible _ = True
checkDifferent (Example x) (Example y) f = f x y

checkIsDefined :: (Answer a) -> (Bool)
checkIsDefined x = case (x) of (Example _) -> True
                               (Impossible) -> True
                               _ -> False
             
defineQ1test = testCase "Q1 is defined" $ assertBool [] $ checkIsDefined q1   
defineQ2test = testCase "Q2 is defined" $ assertBool [] $ checkIsDefined q2   
defineQ3test = testCase "Q3 is defined" $ assertBool [] $ checkIsDefined q3   
defineQ4test = testCase "Q4 is defined" $ assertBool [] $ checkIsDefined q4 
defineQ5test = testCase "Q5 is defined" $ assertBool [] $ checkIsDefined q5   
defineQ6test = testCase "Q6 is defined" $ assertBool [] $ checkIsDefined q6   
defineQ7test = testCase "Q7 is defined" $ assertBool [] $ checkIsDefined q7   
defineQ8test = testCase "Q8 is defined" $ assertBool [] $ checkIsDefined q8   
defineQ9test = testCase "Q9 is defined" $ assertBool [] $ checkIsDefined q9   
defineQ10test = testCase "Q10 is defined" $ assertBool [] $ checkIsDefined q10   
defineQ11test = testCase "Q11 is defined" $ assertBool [] $ checkIsDefined q11 
defineQ12test = testCase "Q12 is defined" $ assertBool [] $ checkIsDefined q12  
defineQ13test = testCase "Q13 is defined" $ assertBool [] $ checkIsDefined q13  
defineQ14test = testCase "Q14 is defined" $ assertBool [] $ checkIsDefined q14  
defineQ15test = testCase "Q15 is defined" $ assertBool [] $ checkIsDefined q15  
defineQ16test = testCase "Q16 is defined" $ assertBool [] $ checkIsDefined q16  
defineQ17test = testCase "Q17 is defined" $ assertBool [] $ checkIsDefined q17    
          
                
--Make sure they didnt just write undefined
definedTests = 
    testGroup 
        "Make sure answers are defined"
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
    testCase "q6 different than q7 (different functions take one same input to differnt outputs)" $ assertBool [] $ checkDifferent q6 q7 (\a1 a2 -> a1 () /= a2 ())
differentQ8Q9 =
    testCase "q8 different than q8 (different functions take one same input to differnt outputs)" $ assertBool [] $ checkDifferent q8 q9 (\a1 a2 -> a1 True False /= a2 True False )


--unfortunately pretty much any other test gives away the answers
