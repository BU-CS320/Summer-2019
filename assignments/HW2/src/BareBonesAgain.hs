module BareBonesAgain where

import Prelude (Show,undefined,Bool(True,False),(&&),(||),not,
                Integer,(==),(/=),(<),(>),(<=),(>=),(+),(-),(*),div,mod)





-- Practice functions on lists


data List a = Nil | Cons a (List a) deriving Show

isEmpty :: List a -> Bool
isEmpty _   = undefined

-- append two lists
(++) :: (List a) -> (List a) -> (List a)
(++) _  _ = undefined

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


-- keep only the elements from the list that have the property of interest, maintain the original order
filter :: (a -> Bool) ->  List a ->  List a
filter  _  _ = undefined

keepEvens :: List Integer ->  List Integer
keepEvens = undefined

-- Pairs

data Pair a b = Pair a b deriving Show


fst :: Pair a b -> a
fst  = undefined

snd :: Pair a b -> b
snd  = undefined

-- zip 2 lists together
-- match as many pairs as possible, you may stop when either list is empty
zip :: (List a) -> (List b) -> List (Pair a b)
zip  _  _ = undefined



-- Maybe

-- sometimes we don't know if we will have a result.  So we can use the "Maybe" datatype.
data Maybe a = Nothing | Just a deriving Show

head :: List a -> Maybe a
head _ = undefined

last :: List a -> Maybe a
last _ = undefined

-- return nth element in list, starting at 0; similar to array indexing
(!!) :: List a -> Integer -> Maybe a
(_)  !! (_) = undefined


