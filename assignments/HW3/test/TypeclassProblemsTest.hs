{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- TODO: this is a mess
module TypeclassProblemsTest where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Test.QuickCheck.Arbitrary.Generic

import Data.Set (Set, fromList, size)
import Test.Tasty.QuickCheck as QC
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

import TypeclassProblems

deriving instance Eq DayOfTheWeek
deriving instance Ord DayOfTheWeek

data Silly = A | B | C Bool deriving (Eq, Generic, Show)
instance AllTheThings Silly where 
	listOfAll = [A, B, C True, C False]
instance Arbitrary Silly where
	arbitrary = genericArbitrary
	shrink = genericShrink

-- This function transform a random pair of int to be between 0 and its corresponding upper bound
transFormRandIJToRanged :: Int -> Int -> (Int, Int) -> (Int, Int)
transFormRandIJToRanged upBoundI upBoundJ (origI, origJ) = (abs . (`mod` upBoundI) $ origI, abs . (`mod` upBoundJ) $ origJ)

enumTest = testGroup "Test for Enum "[
	testCase "`toEnum` of 0 to 6 should not be equal to each other, there should be 7 distinct days" $
	assertEqual [] 7 (size $ fromList (map toEnum [0..6] :: [DayOfTheWeek])),
	testCase "[0..6] == fromEnum (toEnum [0..6] :: [DayOfTheWeek]) " $
	assertEqual [] [0..6] (map fromEnum (map toEnum [0..6] :: [DayOfTheWeek]))
	]

allTheThingsTest = testGroup "Test for AllTheThings" [

	testCase "ListOfAll :: [Bool]` should either be [True, False] or [False, True]" $
	assertBool [] ((listOfAll :: [Bool]) == [True, False] || (listOfAll :: [Bool]) == [False, True]),

	testCase "ListOfAll :: [DayOfTheWeek]` should have the same element as `map toEnum [0..6] " $
	assertEqual [] (Data.Set.fromList (listOfAll :: [DayOfTheWeek])) (Data.Set.fromList (map toEnum [0..6])),

	QC.testProperty "for all index i j, the pair consist of ith element of listOfAll of `Bool` and the jth element of listOfAll of `DayOfTheWeek` should in listOfAll of `(Bool, DayOfTheWeek)`" $
		let 
			boolList = (listOfAll :: [Bool])
			dayList = (listOfAll :: [DayOfTheWeek])
			lenBoolList = length boolList 
			lenDayList = length dayList
			numInRangeIJ = fmap (transFormRandIJToRanged lenBoolList lenDayList) arbitrary :: Gen (Int, Int)
		in
			QC.forAll numInRangeIJ $ \(i, j) -> (boolList !! i, dayList !! j) `elem` (listOfAll :: [(Bool, DayOfTheWeek)]),

	QC.testProperty "for all index i j, the pair consist of ith element of listOfAll of `Silly` (defined as `data Silly = A | B | C Bool`) and the jth element of listOfAll of `DayOfTheWeek` should in listOfAll of `(Bool, DayOfTheWeek)`" $
		let 
			sillyList = (listOfAll :: [Silly])
			dayList = (listOfAll :: [DayOfTheWeek])
			lenSillyList = length sillyList 
			lenDayList = length dayList
			numInRangeIJ = fmap (transFormRandIJToRanged lenSillyList lenDayList) arbitrary :: Gen (Int, Int)
		in
			QC.forAll numInRangeIJ $ \(i, j) -> (sillyList !! i, dayList !! j) `elem` (listOfAll :: [(Silly, DayOfTheWeek)])

	]

tests = testGroup "enum and all the thing test" [
	enumTest,
	allTheThingsTest
	]