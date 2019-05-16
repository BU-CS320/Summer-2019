module Hw where
import Prelude(Show, undefined)

-- Fill in the bodies of the undefined functions and data.
-- DO NOT CHANGE THE TYPE SIGNATURES!


-- Bools may be encoded as data
data Bool = True | False deriving Show

-- Define the following familiar functions on Bools.

not :: Bool -> Bool
not _ = undefined


and :: Bool -> Bool -> Bool
and _ _       = undefined


or :: Bool -> Bool -> Bool
or _ _     = undefined


xor :: Bool -> Bool -> Bool
xor _ _  = undefined


-- natural numbers may be encoded as data
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

add ::  Nat -> Nat -> Nat
add _ _  = undefined

mult ::  Nat -> Nat -> Nat
mult _ _  = undefined


-- exp takes base followed by exponent:  exp two one = two
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

-- Less than or equal
le :: Nat -> Nat -> Bool
le x y = undefined

-- greater than
gt :: Nat -> Nat -> Bool
gt x y = undefined

-- greater than or equal
ge :: Nat -> Nat -> Bool
ge x y = undefined

-- return True on even Nats, False on odd Nats
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
isWeekend  _ = undefined

-- Write a function that gives the next day
nextDay :: DayOfWeek -> DayOfWeek
nextDay  _ = undefined

-- you can encode cartesian coordinates with data
-- Write a data type for a 2D point where x and y are Nats
data Point --  = ...  deriving Show

-- these three functions will help grading
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



-- Assume there is an boring math class where students only answer with a Bool OR with a Nat, 
-- write a data type for that answer
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

-- Important data structures: Lists

-- We can write lists for specific data, let's do Nats first
data ListNat = NilNat | ConsNat Nat ListNat    deriving Show

-- Create a list of the first 4 Nats
exampleListNat :: ListNat
exampleListNat = undefined

-- Find the length of a list (remember length is defined as the number of elements in the list)
lengthOfListNat :: ListNat -> Nat
lengthOfListNat _  = undefined

-- Write a function that finds the sum of all the numbers in the list
sum :: ListNat -> Nat
sum _ = undefined

-- Write a function that tells when 2 Nat lists are equal
eqList :: ListNat -> ListNat -> Bool
eqList _ _ = undefined

-- Write a function that tests when a Nat is in a list
member :: Nat -> ListNat -> Bool
member _ _ = undefined


-- Now let's do lists of Bools

data ListBool = NilBool | ConsBool Bool ListBool deriving Show

-- Give a list containing every bool
exampleListBool :: ListBool
exampleListBool = undefined


lengthOfListBool :: ListBool -> Nat
lengthOfListBool _  = undefined


--  General lists: It gets very tiresome to write a list for every single datatype
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


-- Binary trees

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


-- ungraded bonus questions:

-- What is the smallest datatype you can come up with?
data Smallests -- = ... deriving Show

exampleSmallest = undefined

-- what is the craziest datatype you can come up with?
data Craziests -- = ... deriving Show

exampleCraziests = undefined
