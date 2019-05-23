module HigherOrderTest where

import TestBase 
import HigherOrderProblems (
  addoneList, addoneMap, keepGreaterThan2List, keepGreaterThan2Map,
  sum, product,
  okCat, badCat, mkCat, mkDog, name, isCat, isDog, isHappy, countHappy,
  footballExampleStats, yearAndTeamWithTotalGames,
  yearAndTeamWithMoreThan1Tie, yearAndTeamWithlessThan3Wins, yearsAndTeamWithMoreWinsThanLosses
  )
import qualified Data.Map as Map
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck as QC


addoneListTest = QC.testProperty "For all Lists, the sum before and after addoneList should differ by exactly the length of the list" $
    \l ->   let n = fromIntegral $ length (l::[Integer])
            in Prelude.sum (addoneList l) == n + Prelude.sum (l)

sumTest = QC.testProperty "For all Lists, sum should act like Prelude's sum" $
    \l -> (not $ null (l::[Integer])) QC.==> Prelude.sum l == HigherOrderProblems.sum l
    
productTest = QC.testProperty "For all Lists, product should act like Prelude's product" $ 
    \l -> (not $ null (l::[Integer])) QC.==> (Prelude.product l == HigherOrderProblems.product l)
    

firstnames2 = ["Andre","Xin","Jinye","Marc","Duy","Xichao"]
lastnames2  = ["Pires","He","Cai","Bernstein","Ngyuen","Geng"]
fullnames2  = ["Andre Pires","Xin He","Jinye Cai","Marc Bernstein","Duy Ngyuen","Xichao Geng"]


    
petTests = testGroup "Tests for Pet datatype" [
    QC.testProperty "For all okCat, they are happy when temp >0" $
        \temp -> temp > 0 QC.==> isHappy okCat temp,
        
    QC.testProperty "For all temp, the bad cat will never be happy" $
        \temp -> not $ isHappy badCat temp,
    QC.testProperty "For all temp, a dog should always be happy" $
        \temp -> isHappy (mkDog "good pupper") temp,
        
    QC.testProperty "For all temp and thresh, we make a cat that is happy if the temp is under thresh, then either the cat is happy or temp is larger or equal to thresh" $
        \temp thresh -> (isHappy (mkCat "good neko" (\t -> t<thresh))  temp) || temp>=thresh,
        
    QC.testProperty "For all Cats, you should name them properly" $
        \n -> n == name (mkCat n (\_ -> True)),
        
    QC.testProperty "For all Dogs, you should name them properly" $
        \n -> n == name (mkDog n),
    
    testCase "list of empty pets shouldn't be happy" $ assertEqual [] (0) (countHappy [] 0),
    testCase "list of dogs should all be happy" $ assertEqual [] (3) (countHappy [mkDog "dog1",mkDog "dog2",mkDog "dog3"] 0),
    testCase "list of dogs and cats could be happy" $ assertEqual [] (2) (countHappy [mkDog "dog1",mkCat "cat1" (\t->t==70),mkCat "cat2" (\_->False)] 70)
    ]
    
teamTests = testGroup "tests for team functions" [
    testCase "yearAndTeamWithTotalGames should work" $ assertEqual [] (yearAndTeamWithTotalGames footballExampleStats) ([(1960,"Patriots",14),(1961,"Patriots",14),(1962,"Patriots",14),(1963,"Patriots",14),(1964,"Patriots",14),(1965,"Patriots",14),(1966,"Patriots",14),(1967,"Patriots",14),(1968,"Patriots",14),(1969,"Patriots",14),(1970,"Patriots",14),(1971,"Patriots",14),(1972,"Patriots",14),(1973,"Patriots",14),(1974,"Patriots",14),(1975,"Patriots",14),(1976,"Patriots",14),(1977,"Patriots",14),(1978,"Patriots",16),(1979,"Patriots",16),(1980,"Patriots",16),(1981,"Patriots",16),(1982,"Patriots",9),(1983,"Patriots",16),(1984,"Patriots",16),(1985,"Patriots",16),(1986,"Patriots",16),(1987,"Patriots",15),(1988,"Patriots",16),(1989,"Patriots",16),(1990,"Patriots",16),(1991,"Patriots",16),(1992,"Patriots",16),(1993,"Patriots",16),(1994,"Patriots",16),(1995,"Patriots",16),(1996,"Patriots",16),(1997,"Patriots",16),(1998,"Patriots",16),(1999,"Patriots",16),(2000,"Patriots",16),(2001,"Patriots",16),(2002,"Patriots",16),(2003,"Patriots",16),(2004,"Patriots",16),(2005,"Patriots",16),(2006,"Patriots",16),(2007,"Patriots",16),(2008,"Patriots",16),(2009,"Patriots",16),(2010,"Patriots",16),(2011,"Patriots",16),(2012,"Patriots",16),(2013,"Patriots",16),(2014,"Patriots",16),(2015,"Patriots",16),(2016,"Patriots",16),(2017,"Patriots",16),(2018,"Patriots",16)]),
    testCase "yearAndTeamWithMoreThan1Tie should work" $ assertEqual [] (yearAndTeamWithMoreThan1Tie footballExampleStats) ([(1965,"Patriots"),(1966,"Patriots")]),
    testCase "yearAndTeamWithlessThan3Wins should work" $ assertEqual [] (yearAndTeamWithlessThan3Wins footballExampleStats) ([(1970,"Patriots"),(1981,"Patriots"),(1990,"Patriots"),(1992,"Patriots")]),
    testCase "yearsAndTeamWithMoreWinsThanLosses should work" $ assertEqual [] (yearsAndTeamWithMoreWinsThanLosses footballExampleStats) ([(1961,"Patriots"),(1962,"Patriots"),(1963,"Patriots"),(1964,"Patriots"),(1966,"Patriots"),(1976,"Patriots"),(1977,"Patriots"),(1978,"Patriots"),(1979,"Patriots"),(1980,"Patriots"),(1982,"Patriots"),(1984,"Patriots"),(1985,"Patriots"),(1986,"Patriots"),(1987,"Patriots"),(1988,"Patriots"),(1994,"Patriots"),(1996,"Patriots"),(1997,"Patriots"),(1998,"Patriots"),(2001,"Patriots"),(2002,"Patriots"),(2003,"Patriots"),(2004,"Patriots"),(2005,"Patriots"),(2006,"Patriots"),(2007,"Patriots"),(2008,"Patriots"),(2009,"Patriots"),(2010,"Patriots"),(2011,"Patriots"),(2012,"Patriots"),(2013,"Patriots"),(2014,"Patriots"),(2015,"Patriots"),(2016,"Patriots"),(2017,"Patriots"),(2018,"Patriots")]),
    
    testCase "yearAndTeamWithTotalGames should work when empty" $ assertEqual [] ([]) (yearAndTeamWithTotalGames []), 
    testCase "yearAndTeamWithMoreThan1Tie should work when empty" $ assertEqual [] ([]) (yearAndTeamWithMoreThan1Tie []),
    testCase "yearAndTeamWithlessThan3Wins should work when empty" $ assertEqual [] ([]) (yearAndTeamWithlessThan3Wins []),
    testCase "yearsAndTeamWithMoreWinsThanLosses should work when empty" $ assertEqual [] ([]) (yearsAndTeamWithMoreWinsThanLosses [])
    
    ]
    

tests = testGroup "Test For Higher Order Problem" [
    addoneListTest,
    sumTest,
    productTest,
    petTests
    ]