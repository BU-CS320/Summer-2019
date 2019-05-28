module HigherOrderProblems where

import Map (Map)
import qualified Map as Map
import Prelude hiding (zipWith, sum)
import Data.List hiding (zipWith, sum)
import Data.Function

-- add 1 to each element in a list
addoneList :: [Integer] -> [Integer] 
addoneList = undefined

-- add 1 to each element in a Map 
-- (you will need to finish the Map part of the HW first)
addoneMap :: Ord k => Map k Integer -> Map k Integer
addoneMap = undefined

-- keep only the elements of the list greater than 2 
keepGreaterThan2List :: [Integer] -> [Integer]
keepGreaterThan2List = undefined

-- keep only the elements of the Map greater than 2
-- (you will need to finish the Map part of the HW first)
keepGreaterThan2Map :: Ord k => Map k Integer -> Map k Integer
keepGreaterThan2Map = undefined


-- define sum with foldr
sum :: [Integer] -> Integer
sum xs = foldr undefined undefined xs

-- define product with foldr
product :: [Integer] -> Integer
product xs = foldr undefined undefined xs



-- define a data type for pets, all pets have names (String)
-- Cats are happy depending on the current temperature (Integer)
-- Dogs are always happy
-- Hint: you may need to store a function in the constructor!
-- if you store a function in the constructor you cannot "deriving Show"
data Pet -- = ...

-- define a cat that is happy when the temperature is > 0 and sad when the temperature is <= 0
okCat :: Pet
okCat = undefined

-- define a cat that is never happy
badCat :: Pet
badCat = undefined

-- make a cat from a name and the function that tells how it will be happy
mkCat :: String -> (Integer -> Bool) -> Pet
mkCat = undefined

-- make a dog from a name. 
mkDog :: String -> Pet
mkDog = undefined

-- some functions to help out the graders
name :: Pet -> String
name = undefined

isCat :: Pet -> Bool
isCat = undefined

isDog :: Pet -> Bool
isDog = undefined

-- is a pet happy given the current temperature (Integer)
isHappy :: Pet -> Integer -> Bool
isHappy = undefined

-- How many of your pets are happy?
-- takes in a list of your pets and the current temperature
-- Hint: try to do this in a higher-order way:
--       use map, filter, and length,
--       OR two maps and sum, 
--       OR foldr!
countHappy :: [Pet] -> Integer -> Integer
countHappy = undefined


-- Higher order programming works well for data exploration

-- record of: year, team name, Won, Lost, Ties 
data FootBallStat = FootBallStat Integer String Integer Integer Integer deriving Show

footballExampleStats :: [FootBallStat]
footballExampleStats = [FootBallStat 1960 "Patriots" 5 9 0, FootBallStat 1961 "Patriots" 9 4 1, FootBallStat 1962 "Patriots" 9 4 1, FootBallStat 1963 "Patriots" 7 6 1, FootBallStat 1964 "Patriots" 10 3 1, FootBallStat 1965 "Patriots" 4 8 2, FootBallStat 1966 "Patriots" 8 4 2, FootBallStat 1967 "Patriots" 3 10 1, FootBallStat 1968 "Patriots" 4 10 0, FootBallStat 1969 "Patriots" 4 10 0, FootBallStat 1970 "Patriots" 2 12 0, FootBallStat 1971 "Patriots" 6 8 0, FootBallStat 1972 "Patriots" 3 11 0, FootBallStat 1973 "Patriots" 5 9 0, FootBallStat 1974 "Patriots" 7 7 0, FootBallStat 1975 "Patriots" 3 11 0, FootBallStat 1976 "Patriots" 11 3 0, FootBallStat 1977 "Patriots" 9 5 0, FootBallStat 1978 "Patriots" 11 5 0, FootBallStat 1979 "Patriots" 9 7 0, FootBallStat 1980 "Patriots" 10 6 0, FootBallStat 1981 "Patriots" 2 14 0, FootBallStat 1982 "Patriots" 5 4 0, FootBallStat 1983 "Patriots" 8 8 0, FootBallStat 1984 "Patriots" 9 7 0, FootBallStat 1985 "Patriots" 11 5 0, FootBallStat 1986 "Patriots" 11 5 0, FootBallStat 1987 "Patriots" 8 7 0, FootBallStat 1988 "Patriots" 9 7 0, FootBallStat 1989 "Patriots" 5 11 0, FootBallStat 1990 "Patriots" 1 15 0, FootBallStat 1991 "Patriots" 6 10 0, FootBallStat 1992 "Patriots" 2 14 0, FootBallStat 1993 "Patriots" 5 11 0, FootBallStat 1994 "Patriots" 10 6 0, FootBallStat 1995 "Patriots" 6 10 0, FootBallStat 1996 "Patriots" 11 5 0, FootBallStat 1997 "Patriots" 10 6 0, FootBallStat 1998 "Patriots" 9 7 0, FootBallStat 1999 "Patriots" 8 8 0, FootBallStat 2000 "Patriots" 5 11 0, FootBallStat 2001 "Patriots" 11 5 0, FootBallStat 2002 "Patriots" 9 7 0, FootBallStat 2003 "Patriots" 14 2 0, FootBallStat 2004 "Patriots" 14 2 0, FootBallStat 2005 "Patriots" 10 6 0, FootBallStat 2006 "Patriots" 12 4 0, FootBallStat 2007 "Patriots" 16 0 0, FootBallStat 2008 "Patriots" 11 5 0, FootBallStat 2009 "Patriots" 10 6 0, FootBallStat 2010 "Patriots" 14 2 0, FootBallStat 2011 "Patriots" 13 3 0, FootBallStat 2012 "Patriots" 12 4 0, FootBallStat 2013 "Patriots" 12 4 0, FootBallStat 2014 "Patriots" 12 4 0, FootBallStat 2015 "Patriots" 12 4 0, FootBallStat 2016 "Patriots" 14 2 0, FootBallStat 2017 "Patriots" 13 3 0, FootBallStat 2018 "Patriots" 11 5 0]

-- hint: the following functions are easy if you use map, filter, and pattern matching in a lambda

-- | for instance,
-- >>> yearAndTeamWithTotalGames [FootBallStat 2018 "Rams" 13 	3 	0, FootBallStat 2017 "Rams" 11 	5 	0]
-- [(2018, "Rams", 16), (2017, "Rams", 16)]
yearAndTeamWithTotalGames  :: [FootBallStat] -> [(Integer, String, Integer)] 
yearAndTeamWithTotalGames = undefined

-- | for instance,
-- >>> yearAndTeamWithMoreThan1Tie [FootBallStat 2016 "Seattle Seahawks" 	10 	5 	1 , FootBallStat 2017 "Rams" 11 	5 	0]
-- [(2016, "Seattle Seahawks")]
yearAndTeamWithMoreThan1Tie  :: [FootBallStat] -> [(Integer, String)] 
yearAndTeamWithMoreThan1Tie = undefined

-- | for instance,
-- >>> yearAndTeamWithMoreThan1Tie [FootBallStat 1969 "Patriots" 4 10 0, FootBallStat 1970 "Patriots" 2 12 0]
-- [(1970,"Patriots")]
yearAndTeamWithlessThan3Wins  :: [FootBallStat] -> [(Integer, String)] 
yearAndTeamWithlessThan3Wins = undefined

-- | for instance,
-- >>> yearAndTeamWithMoreThan1Tie [FootBallStat 1960 "Patriots" 5 9 0, FootBallStat 1961 "Patriots" 9 4 1]
-- [(1961,"Patriots")]
yearsAndTeamWithMoreWinsThanLosses :: [FootBallStat] -> [(Integer, String)] 
yearsAndTeamWithMoreWinsThanLosses = undefined


-- ungraded bonus:

wins :: FootBallStat -> Integer
wins = undefined

-- find the record with the most wins each decade
-- Hint you may use (maximumBy (compare `on` wins)) to find the best win record in a list
-- you can use groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
mostWinsEachDecade :: [FootBallStat] -> [FootBallStat]
mostWinsEachDecade xs = undefined
