module HigherOrderTest where

import TestBase 
import HigherOrderProblems (FootBallStat(..),
  addoneList, addoneMap, keepGreaterThan2List, keepGreaterThan2Map,
  sum, product,
  okCat, badCat, mkCat, mkDog, name, isCat, isDog, isHappy, countHappy,
  footballExampleStats, yearAndTeamWithTotalGames,
  yearAndTeamWithMoreThan1Tie, yearAndTeamWithlessThan3Wins, yearsAndTeamWithMoreWinsThanLosses
  )
import Map
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck as QC

tests = testGroup "Test For Higher Order Problem" [
    testProperty "addoneList, For all Lists, the sum before and after addoneList should differ by exactly the length of the list" $
    \l ->   Prelude.sum (addoneList l) == (fromIntegral $ length (l::[Integer])) + Prelude.sum (l), -- TODO: not the best test
	
    testCase "addoneMap (fromList [('a',2), ('b',100)])" $ 
      assertEqual [] (fromList [('a',3), ('b',101)]) $ addoneMap (fromList [('a',2), ('b',100)]),
	  
    testCase "keepGreaterThan2List [1,2,3,-1000,2,100000]" $ 
      assertEqual [] ([3,100000]) $ keepGreaterThan2List [1,2,3,-1000,2,100000],
    testCase "keepGreaterThan2Map (fromList [('a',2), ('b',100), ('c',-100)])" $ 
      assertEqual [] (fromList [('b',100)]) $ keepGreaterThan2Map (fromList [('a',2), ('b',100), ('c',-100)]),
	  
    testProperty "For all Lists, sum should act like Prelude's sum" $
      \l -> Prelude.sum l == HigherOrderProblems.sum l,
    testProperty "For all Lists, product should act like Prelude's product" $ 
      \l -> Prelude.product l == HigherOrderProblems.product l,
    
	testGroup "Tests for Pet datatype" [
      testProperty "For all okCat, is happy when the temperature is > 0 and sad when the temperature is <= 0" $
        \temp -> (temp > 0) == isHappy okCat temp,
        
      testProperty "For all temp, the bad cat will never be happy" $
        \temp -> not $ isHappy badCat temp,
		
      testProperty "For all temp, a dog should always be happy" $
        \temp -> isHappy (mkDog "good pupper") temp,
        
      testProperty "For all temp and thresh, we make a cat that is happy if the temp is under thresh, then either the cat is happy or temp is larger or equal to thresh" $
        \temp thresh -> (isHappy (mkCat "good neko" (\t -> t<thresh))  temp) || temp>=thresh, -- TODO: a bit over complicated
        
      testProperty "For all Cats, you should name them properly" $
        \n -> n == name (mkCat n (\_ -> True)),
        
      testProperty "For all Dogs, you should name them properly" $
        \n -> n == name (mkDog n),
    
      testCase "list of empty pets shouldn't be happy" $ assertEqual [] (0) (countHappy [] 0),
      testCase "list of dogs should all be happy" $ assertEqual [] (3) (countHappy [mkDog "dog1",mkDog "dog2",mkDog "dog3"] 0),
      testCase "list of dogs and cats could be happy" $ assertEqual [] (2) (countHappy [mkDog "dog1",mkCat "cat1" (\t->t==70),mkCat "cat2" (\_->False)] 70)
    ],
	testGroup "tests for team functions" [
    testCase "yearAndTeamWithTotalGames should work" $ assertEqual [] (yearAndTeamWithTotalGames footballExampleStats') ([(1960,"Patriots",14),(1961,"Patriots",14),(1962,"Patriots",14),(1963,"Patriots",14),(1964,"Patriots",14),(1965,"Patriots",14),(1966,"Patriots",14),(1967,"Patriots",14),(1968,"Patriots",14),(1969,"Patriots",14),(1970,"Patriots",14),(1971,"Patriots",14),(1972,"Patriots",14),(1973,"Patriots",14),(1974,"Patriots",14),(1975,"Patriots",14),(1976,"Patriots",14),(1977,"Patriots",14),(1978,"Patriots",16),(1979,"Patriots",16),(1980,"Patriots",16),(1981,"Patriots",16),(1982,"Patriots",9),(1983,"Patriots",16),(1984,"Patriots",16),(1985,"Patriots",16),(1986,"Patriots",16),(1987,"Patriots",15),(1988,"Patriots",16),(1989,"Patriots",16),(1990,"Patriots",16),(1991,"Patriots",16),(1992,"Patriots",16),(1993,"Patriots",16),(1994,"Patriots",16),(1995,"Patriots",16),(1996,"Patriots",16),(1997,"Patriots",16),(1998,"Patriots",16),(1999,"Patriots",16),(2000,"Patriots",16),(2001,"Patriots",16),(2002,"Patriots",16),(2003,"Patriots",16),(2004,"Patriots",16),(2005,"Patriots",16),(2006,"Patriots",16),(2007,"Patriots",16),(2008,"Patriots",16),(2009,"Patriots",16),(2010,"Patriots",16),(2011,"Patriots",16),(2012,"Patriots",16),(2013,"Patriots",16),(2014,"Patriots",16),(2015,"Patriots",16),(2016,"Patriots",16),(2017,"Patriots",16),(2018,"Patriots",16)]),
    testCase "yearAndTeamWithMoreThan1Tie should work" $ assertEqual [] (yearAndTeamWithMoreThan1Tie footballExampleStats') ([(1965,"Patriots"),(1966,"Patriots")]),
    testCase "yearAndTeamWithlessThan3Wins should work" $ assertEqual [] (yearAndTeamWithlessThan3Wins footballExampleStats') ([(1970,"Patriots"),(1981,"Patriots"),(1990,"Patriots"),(1992,"Patriots")]),
    testCase "yearsAndTeamWithMoreWinsThanLosses should work" $ assertEqual [] (yearsAndTeamWithMoreWinsThanLosses footballExampleStats') ([(1961,"Patriots"),(1962,"Patriots"),(1963,"Patriots"),(1964,"Patriots"),(1966,"Patriots"),(1976,"Patriots"),(1977,"Patriots"),(1978,"Patriots"),(1979,"Patriots"),(1980,"Patriots"),(1982,"Patriots"),(1984,"Patriots"),(1985,"Patriots"),(1986,"Patriots"),(1987,"Patriots"),(1988,"Patriots"),(1994,"Patriots"),(1996,"Patriots"),(1997,"Patriots"),(1998,"Patriots"),(2001,"Patriots"),(2002,"Patriots"),(2003,"Patriots"),(2004,"Patriots"),(2005,"Patriots"),(2006,"Patriots"),(2007,"Patriots"),(2008,"Patriots"),(2009,"Patriots"),(2010,"Patriots"),(2011,"Patriots"),(2012,"Patriots"),(2013,"Patriots"),(2014,"Patriots"),(2015,"Patriots"),(2016,"Patriots"),(2017,"Patriots"),(2018,"Patriots")]),
    
    testCase "yearAndTeamWithTotalGames should work when empty" $ assertEqual [] ([]) (yearAndTeamWithTotalGames []), 
    testCase "yearAndTeamWithMoreThan1Tie should work when empty" $ assertEqual [] ([]) (yearAndTeamWithMoreThan1Tie []),
    testCase "yearAndTeamWithlessThan3Wins should work when empty" $ assertEqual [] ([]) (yearAndTeamWithlessThan3Wins []),
    testCase "yearsAndTeamWithMoreWinsThanLosses should work when empty" $ assertEqual [] ([]) (yearsAndTeamWithMoreWinsThanLosses [])
    
    ]
    ]

	-- TODO: update problem to 2019 season
	-- TODO: give them different test data, let them use take?
-- use this to protect editing
footballExampleStats' :: [FootBallStat]
footballExampleStats' = [FootBallStat 1960 "Patriots" 5 9 0, FootBallStat 1961 "Patriots" 9 4 1, FootBallStat 1962 "Patriots" 9 4 1, FootBallStat 1963 "Patriots" 7 6 1, FootBallStat 1964 "Patriots" 10 3 1, FootBallStat 1965 "Patriots" 4 8 2, FootBallStat 1966 "Patriots" 8 4 2, FootBallStat 1967 "Patriots" 3 10 1, FootBallStat 1968 "Patriots" 4 10 0, FootBallStat 1969 "Patriots" 4 10 0, FootBallStat 1970 "Patriots" 2 12 0, FootBallStat 1971 "Patriots" 6 8 0, FootBallStat 1972 "Patriots" 3 11 0, FootBallStat 1973 "Patriots" 5 9 0, FootBallStat 1974 "Patriots" 7 7 0, FootBallStat 1975 "Patriots" 3 11 0, FootBallStat 1976 "Patriots" 11 3 0, FootBallStat 1977 "Patriots" 9 5 0, FootBallStat 1978 "Patriots" 11 5 0, FootBallStat 1979 "Patriots" 9 7 0, FootBallStat 1980 "Patriots" 10 6 0, FootBallStat 1981 "Patriots" 2 14 0, FootBallStat 1982 "Patriots" 5 4 0, FootBallStat 1983 "Patriots" 8 8 0, FootBallStat 1984 "Patriots" 9 7 0, FootBallStat 1985 "Patriots" 11 5 0, FootBallStat 1986 "Patriots" 11 5 0, FootBallStat 1987 "Patriots" 8 7 0, FootBallStat 1988 "Patriots" 9 7 0, FootBallStat 1989 "Patriots" 5 11 0, FootBallStat 1990 "Patriots" 1 15 0, FootBallStat 1991 "Patriots" 6 10 0, FootBallStat 1992 "Patriots" 2 14 0, FootBallStat 1993 "Patriots" 5 11 0, FootBallStat 1994 "Patriots" 10 6 0, FootBallStat 1995 "Patriots" 6 10 0, FootBallStat 1996 "Patriots" 11 5 0, FootBallStat 1997 "Patriots" 10 6 0, FootBallStat 1998 "Patriots" 9 7 0, FootBallStat 1999 "Patriots" 8 8 0, FootBallStat 2000 "Patriots" 5 11 0, FootBallStat 2001 "Patriots" 11 5 0, FootBallStat 2002 "Patriots" 9 7 0, FootBallStat 2003 "Patriots" 14 2 0, FootBallStat 2004 "Patriots" 14 2 0, FootBallStat 2005 "Patriots" 10 6 0, FootBallStat 2006 "Patriots" 12 4 0, FootBallStat 2007 "Patriots" 16 0 0, FootBallStat 2008 "Patriots" 11 5 0, FootBallStat 2009 "Patriots" 10 6 0, FootBallStat 2010 "Patriots" 14 2 0, FootBallStat 2011 "Patriots" 13 3 0, FootBallStat 2012 "Patriots" 12 4 0, FootBallStat 2013 "Patriots" 12 4 0, FootBallStat 2014 "Patriots" 12 4 0, FootBallStat 2015 "Patriots" 12 4 0, FootBallStat 2016 "Patriots" 14 2 0, FootBallStat 2017 "Patriots" 13 3 0, FootBallStat 2018 "Patriots" 11 5 0]

