module MapTest where

import TestBase 
import Map (Map(), empty, insert, toList, fromList, size, member, lookup,
  delete, update, union, filter, null, singleton, elems, keys,
  difference, adjust, alter
  )
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck as QC

import qualified Data.Map as StdMap

-- TODO: clean up the type ascriptions, qualifications many are unneeded
-- TODO: some var names are unneeded
tests = testGroup "Tests for map problems" [
  
  testCase "size of empty should be zero" $
    assertEqual [] 0 $ size empty,
    
  testGroup "Test equal function" [

    testCase "insert 3 'a' (insert 2 'b' empty) == insert 2 'b' (insert  3 'a' empty)" $ 
      assertEqual [] (insert 3 'a' (insert 2 'b' empty)) $ (insert 2 'b' (insert  3 'a' empty)),
      
    testCase " Map.fromList [(5,a), (3,b), (1, c)] == Map.fromList [(3,b), (5,a), (1, c)]" $
      assertEqual "" (fromList [(5,"a"), (3,"b"), (1, "c")]) $ fromList [(3,"b"), (5,"a"), (1, "c")],
      
      
    testProperty "Reflexivity: same map (int-Char pair version) should equal" $
      \ m -> m == (m :: Map Integer Char),
      
    testProperty "Symmetry" $
      \ x y -> (x == (y :: Map Bool Bool)) == (y == x),
      
    testProperty "Transitivity" $
      \ x y z -> ((x == (y :: Map Bool Bool)) && (y == z)) ==> (x == z), 
      
    -- no Substitutivity test
    testProperty "Negation" $
      \ x y -> (x /= (y :: Map Bool Bool)) == (not $ x == y)
      
    ],
    
    
    testGroup "Test insert function" [

        testProperty "insert should behave like Data.Map.insert (int-Char pair version)" $
          \ k v m -> testOneArgFuncByIso
                      (insert (k::Integer) (v::Char))
                      (StdMap.insert k v)
                      m ,

        testProperty "insert should behave like Data.Map.insert (bool-Char pair version)" $
          \ k v m -> testOneArgFuncByIso
                      (insert (k::Bool) (v::Char))
                      (StdMap.insert k v)
                      m,

        testProperty "what have been inserted should be in the map" $
          \ k v m -> (Map.lookup k $ insert (k::Integer) (v::Char) m) == (Just v),
        
        testProperty "given a key exists in the map, insert should change the value if the new value not equals to old value" $ 
          \m k v v' ->  (Map.lookup k $ insert (k::Integer) (v'::Char) $ insert k v m) == (Just v')
    ],
    
    testGroup "Test toList function" [
      QC.testProperty "toList should behave like Data.Map.toList (int-Char pair version)" $
        \ m -> testOneArgFuncByIso
                (Map.toList :: Map.Map Integer Char -> [(Integer, Char)] )
                (StdMap.toList :: StdMap.Map Integer Char -> [(Integer, Char)] )
                m,

      QC.testProperty "toList should behave like Data.Map.toList (Bool-Char pair version)" $
        \ m -> testOneArgFuncByIso
                (Map.toList :: Map.Map Bool Char -> [(Bool, Char)] )
                (StdMap.toList :: StdMap.Map Bool Char -> [(Bool, Char)] )
                m
    ],

    testGroup "Test size function" [
    
      testCase "a specific test for member function: member should return false when i is not in the map" $
        assertBool "" $ not (Map.member 1 $ Map.fromList [(5,'a'), (3,'b')] ),
      
      testCase "another specific test for member function: member should return true when i is not in the map" $
        assertBool "" $ (Map.member 5 $ Map.fromList [(5,'a'), (3,'b')] ),
        
      QC.testProperty "size should behave like Data.Map.size (int-Char pair version)" $
        \ m -> testOneArgFuncByIso 
                (Map.size :: Map.Map Integer Char -> Int)
                (StdMap.size :: StdMap.Map Integer Char -> Int)
                m,

      QC.testProperty "size should behave like Data.Map.size (Bool-Char pair version)" $
        \ m -> testOneArgFuncByIso 
                (Map.size :: Map.Map Bool Char -> Int)
                (StdMap.size :: StdMap.Map Bool Char -> Int)
                m
    ],
    
    testGroup "Test member function" [
      testProperty "member should behave like Data.Map.member (int-Char pair version)" $
        \ m i -> testOneArgFuncByIso 
                  (Map.member i :: Map.Map Integer Char -> Bool)
                  (StdMap.member i :: StdMap.Map Integer Char -> Bool)
                  m,
      
      testProperty "member should behave like Data.Map.member (bool-Char pair version)" $
        \ m b -> testOneArgFuncByIso 
                  (Map.member b :: Map.Map Bool Char -> Bool)
                  (StdMap.member b :: StdMap.Map Bool Char -> Bool)
                  m
    ],
  
  
    testGroup "Test lookup function" [
    
      testCase "test for lookup function: lookup should return Nothing when key is not in the map" $
        assertEqual ""  Nothing  $ (Map.lookup 1 $ Map.fromList [(5,'a'), (3,'b')] ),

      testCase "test for lookup function: member should return true when key is in the map" $
        assertEqual "" (Just 'a') $ (Map.lookup 5 $ Map.fromList [(5,'a'), (3,'b')] ),
      
       testProperty "lookup should behave like Data.Map.lookup (int-Char pair version)" $
          \ m i -> testOneArgFuncByIso 
                    (Map.lookup i :: Map.Map Integer Char -> Maybe Char)
                    (StdMap.lookup i :: StdMap.Map Integer Char -> Maybe Char)
                    m,
       testProperty "lookup should behave like Data.Map.lookup (Bool-Char pair version)" $
          \ m i -> testOneArgFuncByIso 
                    (Map.lookup i :: Map.Map Bool Char -> Maybe Char)
                    (StdMap.lookup i :: StdMap.Map Bool Char -> Maybe Char)
                    m


    ],
    
    testGroup "Test delete function" [
    
      testCase "a specific test for delete function: key is in the map" $
        assertEqual "" (Map.fromList [(3, "b")]) $ Map.delete 5 (Map.fromList [(5,"a"), (3,"b")]),
    
      testCase "another specific test for delete function: key is not in the map" $
        assertEqual "" (Map.fromList [(5,"a"), (3,"b")]) $ Map.delete 7 (Map.fromList [(5,"a"), (3,"b")]) ,
      
      testProperty "delete should behave like Data.Map.delete (int-Char pair version)" $
        \ m i -> testOneArgFuncByIso 
                (Map.delete i :: Map.Map Integer Char -> Map.Map Integer Char)
                (StdMap.delete i :: StdMap.Map Integer Char -> StdMap.Map Integer Char)
                m,
      testProperty "delete should behave like Data.Map.delete (Bool-Char pair version)" $
        \ m i -> testOneArgFuncByIso 
                (Map.delete i :: Map.Map Bool Char -> Map.Map Bool Char)
                (StdMap.delete i :: StdMap.Map Bool Char -> StdMap.Map Bool Char)
                m
    ],
    
    testGroup "Test filter function" [
    
    testCase "a specific test for filter: should return empty" $
       assertEqual "" Map.empty $ Map.filter (> "x") (Map.fromList [(5,"a"), (3,"b")]),
    
    testCase "another specific test for filter: should return something" $
      assertEqual "" (Map.fromList [(3, "b")]) $ Map.filter (> "a") (Map.fromList [(5,"a"), (3,"b")]),
    
    testProperty "filter should behave like Data.Map.filter (int-int pair version)" $
      \ m -> testOneArgFuncByIso 
              (Map.filter (>1) :: Map.Map Integer Integer -> Map.Map Integer Integer)
              (StdMap.filter (>1) :: StdMap.Map Integer Integer -> StdMap.Map Integer Integer)
              m
    ],
    
    testGroup "functor tests" [
      testProperty "For all Lists l, if id function is applied to the list then the result should be the same list" $
        \la -> (fmap id (la::(Map Integer Char))) == la,
        
      testProperty "For all Lists l, fmap (f . g) l == (fmap f . fmap g) l" $ 
        \la -> (fmap ((+1) . (*2)) (la::(Map Integer Integer))) == ((fmap (+1) . fmap (*2)) la) -- TODO: could randomize the functions
    ]
  ]
  