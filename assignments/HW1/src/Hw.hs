module Hw where
import Prelude(Show, undefined)

-- HOMEWORK ONE     Due 2/5 by 11:59pm by upload to your repo
--    (with a couple of hours "grace period" before it is considered late)
--    Note: There is also an analytical part to the homework
--    which will be posted on the class web page, and due to be
--    uploaded to Gradescope with the same due date and time, and
--    with the same grace period. 

-- Fill in the bodies of the undefined functions and data.
-- DO NOT CHANGE THE TYPE SIGNATURES!

-- Think about whether you need to write each function
-- recursively on the structure of the data, or can
-- define more simply in terms of previously-defined functions,
-- or use a helper function. 
-- You may always add your own helper functions and helper data!

-- Remember: Constructors must be capitalized; variable
-- and function names must be in lower case. 
-- Constructor constants are like 0-ary functions (no arguments). 


-- Part A: Basic Boolean data and functions

-- Note on data declarations: "deriving Show" will allow 
-- data values to be printed by interpreter.


data Bool = True | False     deriving Show

-- Define the following familiar functions on Bools.
-- You may need multiple cases for each one.

not :: Bool -> Bool
not _ = undefined


and :: Bool -> Bool -> Bool
and _ _       = undefined


or :: Bool -> Bool -> Bool
or _ _     = undefined


xor :: Bool -> Bool -> Bool
xor _ _  = undefined


-- Part B: Encoding of natural numbers using data expressions
-- and defining basic functions on these expressions.

data Nat =  Zero | Succ Nat deriving Show

-- the first 6 numbers
zero :: Nat
zero = Zero

one :: Nat
one = Succ zero

two :: Nat
two = Succ one

three  :: Nat
three = undefined

four  :: Nat
four = undefined

five  :: Nat
five = undefined


-- Write the following functions
-- (Hint: try recursing on structure of first argument)

add ::  Nat -> Nat -> Nat
add _ _  = undefined

mult ::  Nat -> Nat -> Nat
mult _ _  = undefined

exp ::  Nat -> Nat -> Nat
exp _ _  = undefined

-- When are 2 Nats equal? 
eq :: Nat -> Nat -> Bool
eq _ _  = undefined

-- When are 2 Nats not equal?  
ne :: Nat -> Nat -> Bool
ne _ _  = undefined

-- Less than on Nats
lt :: Nat -> Nat -> Bool
lt _ _  = undefined

-- Remaining Boolean tests

le :: Nat -> Nat -> Bool
le x y = undefined

gt :: Nat -> Nat -> Bool
gt x y = undefined

ge :: Nat -> Nat -> Bool
ge x y = undefined

-- Example of useful test on Nats
-- return True on even Nats, False on odd.
isEven :: Nat -> Bool
isEven _ = undefined

--Return the maximum of two Nats
max :: Nat -> Nat -> Nat
max _ _  = undefined


-- Part C:  Data Expressions: Now let's write our own data.

-- C.1: Dates

-- Write a data type for the 7 days of the week
data DayOfWeek -- = ...  deriving Show

-- what is your favorite Day? (Your choice!)
favoriteDay :: DayOfWeek
favoriteDay = undefined

-- write a function that returns true if it is a weekend
isWeekend :: DayOfWeek -> Bool
isWeekend  _     = undefined

-- Write a function that gives the next day
nextDay :: DayOfWeek -> DayOfWeek
nextDay  _     = undefined

-- write a data type for the Months of the year
data Month -- = ...  deriving Show

-- In which month is your birthday?
partyMonth :: Month
partyMonth = undefined


-- Write a function that gives the next Month
nextMonth :: Month -> Month
nextMonth  _     = undefined


-- C.2: Cartesian Coordinates

-- Write a data type for a 2D point where x and y are Nats
data Point --  = ...  deriving Show

-- Take 2 Nats and construct a Point
makePoint :: Nat -> Nat -> Point
makePoint _ _ = undefined

-- Select components from a Point
getX :: Point -> Nat
getX _ = undefined

getY :: Point -> Nat
getY _ = undefined


-- The Manhattan distance is the distance in the x direction plus the distance in the y direction
-- for instance the Manhattan distance of points (2,5) and (3,1) is 5
manhattanDistance :: Point -> Point -> Nat
manhattanDistance _ _ = undefined


-- C.3: More Alterative data

-- Assume there is an boring math class where students only answer with a Bool OR with a Nat, 
-- write a data type for that answer (hint: you may use two alternatives with | ) 
data ShortAnswer -- = ...  deriving Show

-- Make a Nat answer
answerNat :: Nat -> ShortAnswer
answerNat _ = undefined

-- Make a Bool answer
answerBool :: Bool -> ShortAnswer
answerBool _ = undefined

-- What is 100 - 99?
ans1 :: ShortAnswer
ans1 = undefined

-- Is 100 - 99 an odd number?
ansTrue :: ShortAnswer
ansTrue = undefined

-- If the answers are equal return true otherwise return false
gradeAnswer :: ShortAnswer -> ShortAnswer -> Bool
gradeAnswer _ = undefined

-- Part D: Important data structures: Lists

-- D.1: Lists of Nats

-- We can write lists for specific data, let's do Nats first
data ListNat = NilNat | ConsNat Nat ListNat    deriving Show

-- Create a list of the first 4 nats
exampleListNat :: ListNat
exampleListNat = undefined

-- Find the length of a list (remember length is defined as the number of elements in the list)
lengthOfListNat :: ListNat -> Nat
lengthOfListNat _  = undefined

-- Write a function that finds the sum of all the numbers in the list
sum :: ListNat -> Nat
sum _     = undefined

-- Write a function that tells when 2 Nat lists are equal
eqList :: ListNat -> ListNat -> Bool
eqList _ _  = undefined

-- Write a function that tests when a Nat is in a list
member :: Nat -> ListNat -> Bool
member _ _                = undefined


-- D.2:  Now let's do lists of Bools

data ListBool = NilBool | ConsBool Bool ListBool deriving Show

-- Give a list containing every bool
exampleListBool :: ListBool
exampleListBool = undefined


lengthOfListBool :: ListBool -> Nat
lengthOfListBool _  = undefined


-- D.3:  General lists: It gets very tiresome to write a list for every single datatype
-- so let's abstract out the type of elements using a polymorphic type

data List a = Nil | Cons a (List a)    deriving Show

-- Write a list of all the Bool values
listOfBool :: (List Bool)
listOfBool = undefined

-- Write a list of the first three Nats
listOfNat :: (List Nat)
listOfNat = undefined

-- Write a list of all the weekdays
listOfWork :: (List DayOfWeek)
listOfWork = undefined

-- Useful function on lists
length :: (List a) -> Nat
length _  = undefined

-- Part E: Binary trees

-- A binary tree is either empty, or a node with a left subtree
-- a value at the root and a right subtree
data Tree a = Null | Node (Tree a) a (Tree a)     deriving Show

-- Give a balanced tree of three Bools corresponding to
--             True
--            /    \
--        False    False

exampleTree :: Tree Bool
exampleTree = undefined

-- return the number of elements in the tree
size :: (Tree a) -> Nat
size  _  = undefined

-- Return the height (= number of nodes in longest path from root to leaf)

height :: (Tree a) -> Nat
height  _  = undefined

-- Do an inorder traversal and store elements in a list
inorder :: (Tree a) -> (List a)
inorder  _  = undefined

-- Do a preorder traversal
preorder :: (Tree a) -> (List a)
preorder  _  = undefined


-- extra ungraded questions below

-- What is the smallest datatype you can come up with?
data Smallests -- = ... deriving Show

exampleSmallest = undefined

-- what is the craziest datatype you can come up with?
data Craziests -- = ... deriving Show

exampleCraziests = undefined
