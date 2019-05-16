{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module ListTests where

import TestBase
import Hw02 (isEmpty, length, (++), addToEnd, reverse, concat, take, map, multiplyEachBy7, filter, fst, snd, zip, head, last, (!!))
import Data.List
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck as QC



--ToDO emptyTestGroup w/ emptyTest1 & emptyTest2

--ToDo 
emptyTestGroup = testGroup "Tests for empty function"
    [
    emptyTest1,
    emptyTest2
    ]
    
emptyTest1 =  testCase "Empty list is empty" $ assertBool [] (isEmpty Nil)
                                               
emptyTest2 = QC.testProperty "For all Lists, it is not empty if it is not Nil" $
    \l -> (not $ null (l::[Integer])) QC.==> not $ isEmpty $ p2l l
    
    
    
appendTest = QC.testProperty "For all lists, append should equal Prelude's append" $
    \la lb ->  (la::[Student]) Data.List.++ (lb::[Student])  ==  (l2p ((p2l la) Hw02.++ (p2l lb)))

addToEndTest = QC.testProperty "For all lists, addToEnd should be equivilant to (x:xs)++[b]" $
    \la b -> (la::[Student]) Data.List.++ [b] == (l2p $ addToEnd b (p2l la))
    
reverseTest = QC.testProperty "For all lists, reverse should equal Prelude's reverse" $
    \la ->  (Hw02.reverse la) == (p2l $ Data.List.reverse $ l2p (la::List Student))

concatTest = QC.testProperty "For all lists, concat should equal Prelude's concat" $ 
    \la -> (Prelude.concat la :: [Bool]) == (l2p $ Hw02.concat $ p2l (Prelude.map p2l la))
    
    
takeTest = QC.testProperty "For all lists, Take should equal Prelude's Take" $
    \n la -> n >= 0 QC.==> (Hw02.take n la) == (p2l $ Data.List.take (fromIntegral n) $ l2p (la::List Bool))

mapTest = QC.testProperty "For all lists, Map should equal Prelude's Map" $
    \la -> (Hw02.map not la) == (p2l $ Data.List.map not $ l2p (la::List Bool))
    

--translate from List to Prelude
l2p :: List a -> [a]
l2p Nil = []
l2p (Cons x xs) = x:(l2p xs)

--translate from Prelude to List
p2l :: [a] -> List a
p2l [] = Nil
p2l (x:xs) = Cons x (p2l xs)


--appendTest = 
---- Test for Lists ----
listTests = testGroup "tests for Lists and List operations" 
    [
        emptyTestGroup,
        appendTest,
        addToEndTest,
        concatTest,
        reverseTest,
        takeTest,
        mapTest
    ]
    