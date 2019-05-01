module Set(Set(),
  size,
  empty,
  singleton,
  insert,
  fromList,
  delete,
  member,
  elems
   ) where

import Prelude (Show,undefined,Bool(True,False),(&&),(||),not,
                Integer,(==),(/=),(<),(>),(<=),(>=),(+),(-),(*),div,mod)
import qualified  Prelude ((++), show)
import Hw02 hiding (insert) 


-- a set based on search trees
-- note that the implementation details are hidden inside the module
-- this means that outside the module a bad Set Cannot be defined

data Set e = Set (Ordering e) (SetInner e)

data SetInner e = Null | Node (SetInner e) e (SetInner e) deriving Show

-- someone outside the module can make good sets from the functions we provide
exampleGood :: Set Integer
exampleGood = insert 3 ( insert 2 ( insert 1 (empty intOrd)))

-- only possible to make these mistakes inside the module
exampleBad :: Set Integer
exampleBad = Set intOrd (Node (Node Null 3 Null) 2 (Node Null 2 Null) )


-- The number of elements in the set.
size :: Set e -> Integer
size = undefined

-- The empty set.
empty ::  (Ordering e) -> Set e
empty  = undefined

-- Create a singleton set.
singleton :: Ordering e -> e -> Set e
singleton = undefined


-- Insert an element in a set. If the set already contains an element equal to the given value, it is replaced with the new value.
insert :: e -> Set e -> Set e
insert  = undefined
 
-- Create a set from a list of elements.
fromList :: Ordering e -> List e -> Set e
fromList = undefined

--  The elements of a set in ascending order.
elems  :: Set e -> List e
elems = undefined

-- Is the element in the set?
member :: e -> Set e -> Bool
member = undefined


-- ungraded challenge problem:
-- Delete an element from a set.
delete :: e -> Set e -> Set e
delete = undefined


-- ignore this for now, just so it is pretty in your repl
instance Show e => Show (Set e) where 
  show (Set _ inner) = (Prelude.++) ((Prelude.++) "Set ??? ("  (Prelude.show  inner)) ")"
  
  