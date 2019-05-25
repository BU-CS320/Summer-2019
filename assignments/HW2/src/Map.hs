module Map
  (Map(),empty, insert, toList, fromList, size, member, lookup, delete, filter,
  update, union, null, singleton, elems, keys, difference, adjust, alter
  ) where  -- DO NOT EDIT THESE LINES
import Prelude hiding (filter, lookup, null)
  
  
-- For this problem, you must develop a variation of a dictionary based on BST. 
-- Such a data structure is very useful in a wide variety of applications.


data Map k v = Null | Node (Map k v) k v (Map k v)  deriving Show

-- The empty map.
empty :: Map k a
empty = undefined

-- Insert a new key and value in the map. If the key is already present in the map, the associated value is replaced with the supplied value
insert :: Ord k => k -> a -> Map k a -> Map k a
insert = undefined

-- Convert to a list of key/value pairs.
-- returns a list where keys are sorted from smallest to the largest
toList :: Map k a -> [(k, a)]
toList = undefined

-- Build a map from a list of key/value pairs.
fromList :: Ord k => [(k, a)] -> Map k a
fromList [] = empty
fromList ((k,a):rest) = insert k a (fromList rest)

-- The number of elements in the map.
size :: Map k a -> Int
size = undefined

-- Is the key a member of the map? 
member :: Ord k => k -> Map k a -> Bool
member = undefined

-- Lookup the value at a key in the map.
-- The function will return the corresponding value as (Just value), or Nothing if the key isn't in the map.
lookup :: Ord k => k -> Map k a -> Maybe a
lookup = undefined

-- Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
delete :: Ord k => k -> Map k a -> Map k a
delete = undefined


-- Filter all values that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter = undefined


instance (Ord k, Eq v) => Eq (Map k v) where
  x == y = undefined

  
instance Functor (Map k) where
-- Map a function over all values in the map.
--  fmap ::  (a -> b) -> Map k a -> Map k b
  fmap f _ = undefined
  
  

-- ungraded bonus

-- The expression (update f k map) updates the value x at k (if it is in the map). If (f x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y.
update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update = undefined

--  The expression (union t1 t2) takes the left-biased union of t1 and t2. It prefers t1 when duplicate keys are encountered, i.e. (union == unionWith const).
union :: Ord k => Map k a -> Map k a -> Map k a
union = undefined

-- Is the map empty?
null :: Map k a -> Bool
null = undefined

-- A map with a single element.
singleton :: k -> a -> Map k a
singleton = undefined

-- Return all elements of the map in the ascending order of their keys.
elems :: Map k a -> [a]
elems = undefined

-- Return all keys of the map in ascending order.
keys :: Map k a -> [k]
keys = undefined

-- Difference of two maps. Return elements of the first map not existing in the second map.
difference :: Ord k => Map k a -> Map k b -> Map k a
difference = undefined

-- Update a value at a specific key with the result of the provided function. When the key is not a member of the map, the original map is returned.
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust = undefined

-- The expression (alter f k map) alters the value x at k, or absence thereof. alter can be used to insert, delete, or update a value in a Map. In short : lookup k (alter f k m) = f (lookup k m).
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = undefined


-- instance (Ord k, Ord v) => Ord (Map k v) where
  

