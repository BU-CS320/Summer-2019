-- This file is just for hints, examples, and test cases
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module MapHints () where

-- In a non empty Map this function will return a new map with the smallest element removed and the smallest element
-- In an empty map this function will return Nothing

-- This can help efficient deletion
pullMin :: Ord k => Map k v -> Maybe (Map k v, k, v)
pullMin Null = Nothing
pullMin (Node Null k v r) = Just (r, k, v) -- smallest thing
pullMin (Node l k v r) = case pullMin l of
  Just (l', k, v) -> Just ((Node l' k v r), k, v)
  Nothing -> Nothing -- this case should be impossible














  
  
-- ignore this, just so the file compiles
data Map k v = Null | Node (Map k v) k v (Map k v)  deriving Show
