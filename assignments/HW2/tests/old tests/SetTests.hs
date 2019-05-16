module SetTests where 

import TestBase
import Set as S hiding (Set)
import Data.Set 
import Hw02  (intOrd, boolOrd, length, (!!), Maybe(..))
import Data.List
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, (@=?))
import Test.Tasty.QuickCheck as QC

---- Test for Sets ----
setTests = testGroup "tests for Set functions" 
    [
        setTestEmpty,
        setTestSingleton,
        setTestSingletonMember,
        setTestFromList,
        setRepeatTest,
        setSizeTest
    ]

crazyIntOrd :: TestBase.Ordering Integer
crazyIntOrd = Ordering (\n m -> EqualTo)

--test if the empty set size is zero
setTestEmpty = QC.testProperty "Empty set should have size 0" $ 
    0 == S.size (S.empty intOrd)

--test if the singleton list is size one 
setTestSingleton = QC.testProperty "The size of the singleton list should be one" $ 
    \n -> 1 == S.size (S.singleton Hw02.intOrd n)

--test if the member of a singleton list is the same as the one added
setTestSingletonMember = QC.testProperty "The elem of a singleton list m should be the same as the one being inserted n" $ 
    \n -> S.member n (S.singleton intOrd n)

--test memeber by turning a list into a set 
setTestFromList = QC.testProperty "for all list, converting the list to set, the size of the set should smaller or equal to the size of the list" $
    \l -> Hw02.length l >= (S.size $ S.fromList intOrd l)

-- test on a crazy order
setTestCrazyOrd = QC.testProperty "for all list if the ordering on the list is everything equal to everything else, then the set made from that list have size 1" $
    \l -> (S.size $ S.fromList crazyIntOrd l) == 1

--test for repeat insertion
setRepeatTest = QC.testProperty "for all list l and integer i, we insert element of index i into the set created by l, the set should not change" $ 
    \l i-> i < (Hw02.length l) QC.==>
         let 
            setFromL = S.fromList Hw02.intOrd l 
         in case (l Hw02.!! i) of
            Hw02.Nothing -> True
            Hw02.Just n -> (S.insert n setFromL) == setFromL

-- each elements in elems should be a member of set
setElemsIsMemberTest = QC.testProperty "for all set s, evey elements of elems s should be member of s"$
    \l -> let 
            s = S.fromList intOrd l 
            elementsOfS = S.elems s 
          in 
            all (\e -> S.member e s) $ l2p elementsOfS 

-- if you insert an element into the set, set should have that element 
setInsertExistsBool = QC.testProperty "forall set of bool s and bool b, if you insert b into s, b should be member of s" $
    \l b -> let 
            insertedSet = S.insert b (S.fromList boolOrd l)
        in 
            S.member b insertedSet

setInsertExistsInt = QC.testProperty "forall set of int s and int n, if you insert n into s, n should be member of s" $
    \l n -> let 
            insertedSet = S.insert n (S.fromList intOrd l)
        in 
            S.member n insertedSet

-- if you delete an element from the set, the element shouldn't be in the set
setDeleteNonExistsInt = QC.testProperty "for a int list l and index i, if you make a set s from that list, after deleting the ith element of l from set, the element should not exists in the set" $
    \l i -> i < (Hw02.length l) QC.==>
        let 
            setFromL = S.fromList Hw02.intOrd l 
        in case (l Hw02.!! i) of
            Hw02.Nothing -> True
            Hw02.Just n -> not . S.member n $ (S.delete n setFromL) 

setDeleteNonExistsBool = QC.testProperty "for a bool list l and index i, if you make a set s from that list, after deleting the ith element of l from set, the element should not exists in the set" $
    \l i -> i < (Hw02.length l) QC.==>
        let 
            setFromL = S.fromList Hw02.boolOrd l 
        in case (l Hw02.!! i) of
            Hw02.Nothing -> True
            Hw02.Just n -> not . S.member n $ (S.delete n setFromL) 
                
--test size compare size of student set vs Data.Set
setSizeTest = QC.testProperty "for all sets s, the size of the set should be the same as the size of the set in the built in Data.Set module" $
    \l -> S.size (S.fromList Hw02.intOrd l) == toInteger (Data.Set.size (Data.Set.fromList (l2p l)))




--translate from List to Prelude
l2p :: List a -> [a]
l2p Nil = []
l2p (Cons x xs) = x:(l2p xs)


