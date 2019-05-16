module OrderingTests where 

import TestBase (List(..), Ordering(..), Comparison(..))
import Hw02 (compareInteger, compareBool, intOrd, boolOrd, insert, sort)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, (@=?))
import Test.Tasty.QuickCheck as QC
import Data.List (sort)

reverse' :: Integer -> Integer -> Comparison
reverse' a b  | a > b = LessThan
              | a < b = GreaterThan
              | a == b = EqualTo

intOrd' :: TestBase.Ordering Integer
intOrd' = TestBase.Ordering reverse'

orderingTests = testGroup "tests for compare data" 
    [
        QC.testProperty "standard int ord" $ 
            \ a b -> case compareInteger a b of
                         LessThan -> a < b
                         EqualTo -> a == b
                         GreaterThan -> a > b,
        QC.testProperty "standard bool ord" $ 
            \ a b -> case compareBool a b of
                         LessThan -> a < b
                         EqualTo -> a == b
                         GreaterThan -> a > b,


        QC.testProperty "insert works, when sorting is considered greatest to least" $ 
            \ ls i ->
              let
                sortedList = reverse $ Data.List.sort (ls :: [Integer])
                result = l2p $ insert intOrd' i $ p2l sortedList
                correct = reverse $ Data.List.sort (i:ls)
              in result == correct ,
        QC.testProperty "sort works, when sorting is considered greatest to least" $ 
            \ ls ->
              let
                correct = reverse $ Data.List.sort (ls :: [Integer])
                result = l2p $ Hw02.sort intOrd' $ p2l ls
              in result == correct 
    ]
    

--translate from List to Prelude
l2p :: List a -> [a]
l2p Nil = []
l2p (Cons x xs) = x:(l2p xs)

--translate from Prelude to List
p2l :: [a] -> List a
p2l [] = Nil
p2l (x:xs) = Cons x (p2l xs)

app :: TestBase.Ordering a -> (a -> a -> Comparison)
app (Ordering ord) = ord
