module StudentTests where 

import TestBase (List(..), Pair(..), Maybe(..), Ordering(..), Comparison(..), Student(..))
import Hw02 (mkCsStudent, mkMathStudent, isCs, getBuId, getYear, coolestStudent, groupProject, studentOrd)
import Data.Typeable
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, (@=?))
import Test.Tasty.QuickCheck as QC

---- Test for Integers ----
studentTests = testGroup "tests for Student functions" 
    [
    -- check isCS, mkCSsthdent
        QC.testProperty "for all id, year, whether takes cs320 or not, if we make a cs student, he/she shoube be a cs student" $ 
            \id year cs320 -> isCs (mkCsStudent id year cs320),
        QC.testProperty "for all id, if we made a cs student, his/her id must be the same as input" $ 
            \id year cs320 -> getBuId (mkCsStudent id year cs320) == id,
        QC.testProperty "for all year, if we made a cs student , his/her year must be the same as input" $ 
            \id year cs320 ->  getYear (mkCsStudent id year cs320) == year,
    -- check makeMathStudent
        QC.testProperty "for all year, if we made a cs student , his/her year must be the same as input" $ 
            \id year fr ->  getYear (mkMathStudent id year fr) == year,
        QC.testProperty "or all id, if we made a cs student, his/her id must be the same as input" $ 
            \id year fr -> getBuId (mkMathStudent id year fr) == id,
            
        QC.testProperty "for all id, year, friends number, if we create a list of math student using these info, there should be no cool student" $ 
            \ ls -> let
                      mathLs = fmap (\ (id, year, fr) -> (mkMathStudent id year fr)) (ls :: [(Integer,Integer,Integer)])
                    in coolestStudent (p2l mathLs) == TestBase.Nothing,
                    
        QC.testProperty "for a list with only math student and cs student without taking cs320, then there should be no cool student" $ 
            \ mls cls -> 
                    let
                      csLs = fmap (\ (id, year) -> (mkCsStudent id year False)) (cls :: [(Integer,Integer)])
                      mathLs = fmap (\ (id, year, fr) -> (mkMathStudent id year fr)) (mls :: [(Integer,Integer,Integer)])
                    in coolestStudent (p2l (mathLs ++ csLs)) == TestBase.Nothing,
        QC.testProperty "sc 320 students are cool" $ 
            \ mls cls id year rest -> 
                    let
                      coolkid = (mkCsStudent id year True)
                      csLs = fmap (\ (id, year) -> (mkCsStudent id year False)) (cls :: [(Integer,Integer)])
                      mathLs = fmap (\ (id, year, fr) -> (mkMathStudent id year fr)) (mls :: [(Integer,Integer,Integer)])
                    in coolestStudent (p2l (mathLs ++ csLs ++ [coolkid] ++ (rest :: [Student]))) == TestBase.Just coolkid,
					

        QC.testProperty "groupProject: pairs all have exactly 1 CS student" $   -- TODO nest tests better
            \ students -> 
                    let
                      result = fmap (\ (Pair a b) -> [a,b]) $ l2p $ groupProject $ p2l $ (students :: [Student])
                    in 	all (\ p -> (length $ filter (isCs) p)  == 1) result  ,
					
        QC.testProperty "groupProject: length is equal to the least of math or CS " $ 
            \ mls cls  -> 
                    let
                      csLs = fmap (\ (id, year) -> (mkCsStudent id year False)) (cls :: [(Integer,Integer)])
                      mathLs = fmap (\ (id, year, fr) -> (mkMathStudent id year fr)) (mls :: [(Integer,Integer,Integer)])
                      result = fmap (\ (Pair a b) -> [a,b]) $ l2p $ groupProject $ p2l $ (csLs ++ mathLs)
                    in (length result) == min (length csLs) (length mathLs),
					
        QC.testProperty "for all list of student, after assigning to groupProjects, there shouldn't be any student not in the original list" $ 
            \ students -> 
                    let
                      result = fmap (\ (Pair a b) -> [a,b]) $ l2p $ groupProject $ p2l $ (students :: [Student])
                    in 	all (\ s -> elem s students) $ concat $ result  ,

        QC.testProperty "for all student1 and student2, student1.id < student2.id, the order of these two students should be student1 < student2" $ 
            \ s1 s2 -> case (app studentOrd) s1 s2 of
                         LessThan -> (getBuId s1) < (getBuId s2)
                         EqualTo -> (getBuId s1) == (getBuId s2)
                         GreaterThan -> (getBuId s1) > (getBuId s2)
                         
    ]

app :: TestBase.Ordering a -> (a -> a -> Comparison)
app (Ordering ord) = ord

    

--translate from List to Prelude
l2p :: List a -> [a]
l2p Nil = []
l2p (Cons x xs) = x:(l2p xs)

--translate from Prelude to List
p2l :: [a] -> List a
p2l [] = Nil
p2l (x:xs) = Cons x (p2l xs)

