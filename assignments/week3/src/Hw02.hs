module Hw02 where

import Prelude (Show,undefined,Bool(True,False),(&&),(||),not,
                Integer,(==),(/=),(<),(>),(<=),(>=),(+),(-),(*),div,mod)

        
        
-- Part A: Practice defining functions on Integers
--         using if-then-else, == on integers, guards,
--         and where clauses to hide helper functions,
--         plus have all normal functions on Integers. 

-- fibonnaci
-- example: fib 0 = 0
fib :: Integer -> Integer
fib _ = undefined

-- Greatest Common Divisor

gcd :: Integer -> Integer -> Integer
gcd _ _ = undefined



-- Part B: Practice functions on lists


data List a = Nil | Cons a (List a) deriving Show

isEmpty :: List a -> Bool
isEmpty _   = undefined

length :: List a -> Integer
length _   = undefined

-- Concatenate two lists, make sure it works
-- if either or both are empty
-- Hint: structural induction on first argument
-- you may have done this in the last hw
(++) :: (List a) -> (List a) -> (List a)
(++) _  _ = undefined

-- Add an element to the end of the list
addToEnd :: a -> (List a) -> (List a)
addToEnd _  _ = undefined

-- reverse a list
reverse :: List a -> List a
reverse _ = undefined

-- flatten a list of lists
concat :: List (List a) -> List a
concat _ = undefined

-- return the first n elements
take :: Integer -> List a -> List a
take _  _ = undefined


-- some "higher order" functions

-- apply the function to each member of the list
map :: (a -> b) ->  List a ->  List b
map = undefined

multiplyEachBy7 :: List Integer -> List Integer
multiplyEachBy7 = undefined


-- keep only the elements from the list that have the property of interest
filter :: (a -> Bool) ->  List a ->  List a
filter  _  _ = undefined

keepEvens :: List Bool ->  List Bool
keepEvens = undefined

-- Pairs

data Pair a b = Pair a b deriving Show


fst :: Pair a b -> a
fst  = undefined

snd :: Pair a b -> b
snd  = undefined

-- zip 2 lists together
zip :: (List a) -> (List b) -> List (Pair a b)
zip  _  _ = undefined



-- Maybe

-- sometimes we don't know if we will have a result.  So we can use the "Maybe" datatype.

data Maybe a = Nothing | Just a deriving Show

head :: List a -> Maybe a
head _ = undefined

last :: List a -> Maybe a
last _ = undefined


-- Next we define an infix function

-- return nth element in list, starting at 0; similar to array indexing
(!!) :: List a -> Integer -> Maybe a
(_)  !! (_) = undefined


-- sorting 

data Comparison = LessThan | EqualTo | GreaterThan deriving Show

compareInteger :: Integer -> Integer -> Comparison
compareInteger = undefined

-- when false < true
compareBool :: Bool -> Bool -> Comparison
compareBool = undefined

-- when we have frequently used functions, we can wrap them in data
data Ordering a = Ordering (a -> a -> Comparison)

intOrd :: Ordering Integer
intOrd = Ordering compareInteger

boolOrd :: Ordering Bool
boolOrd = Ordering compareBool

-- let's write insertion sort

-- inserts an a into a sorted list of a (the list is sorted least to greatest)
-- for example: insert intOrd 3 (Cons 1 (Cons 4 Nil)) = (Cons 1 (Cons 3 (Cons 4 Nil)))
insert :: Ordering a -> a -> List a -> List a
insert = undefined


-- sort the list
sort :: Ordering a -> List a -> List a
sort = undefined


-- write a datatype representing a student, the student should have a bu-ID, current year
-- CS students should have a Bool (are they taking 320)
-- Math students have an Integer  (how many friends do they have)

data Student -- = ...  deriving Show

-- from buid, year, and if they are taking 320
-- for instance a freshman in this class would be created by, "mkCsStudent 12345678 0 True"
mkCsStudent :: Integer ->  Integer ->  Bool -> Student
mkCsStudent = undefined


-- from buid, year, and how many friends they have
mkMathStudent :: Integer ->  Integer ->  Integer -> Student
mkMathStudent = undefined

-- is this a CS student? (this is to make testing easier)
isCs :: Student ->  Bool
isCs = undefined

getBuId :: Student ->  Integer
getBuId = undefined

getYear :: Student ->  Integer
getYear = undefined

-- coolestStudent is defined to be the first CS student who is taking 320 in this list
coolestStudent :: List Student -> Maybe Student
coolestStudent = undefined

-- the students need to be put into pairs for group projects.
-- Take the list of students and create pairs of math and CS students from the list.
-- If possible every Math student should be paired with a CS student.
-- If some students are not paired up that is ok.
groupProject :: List Student -> List (Pair Student Student)
groupProject ls = undefined


-- define an ordering on students based entirely on bu-ID, students with the same BU id are "EqualTo" regardless of other information
studentOrd :: Ordering Student
studentOrd = undefined


