module MapTest where

  import TestBase 
  import Map (Map(), empty, insert, toList, fromList, size, member, lookup,
    delete, update, union, filter, null, singleton, elems, keys,
    difference, adjust, alter
    )
  import Test.Tasty (testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
  import Test.Tasty.QuickCheck as QC
  import Data.Map

  ---- All Map Test ----
  mapTest = testGroup "Test For Map Problem" [
      emptyTest,
      fromListTest,
      insertTest,
      toListTest,
      sizeTest,
      memberTest,
      lookupTest,
      deleteTest,
      updateTest,
      unionTest,
      filterTest,
      equalTest
    ]
  
  emptyTest = testGroup "Test empty function" [
    QC.testProperty "size of empty map should be zero" $ 0 == Map.size Map.empty,
    
    testCase "empty map should not have any element" $ assertEqual [] [] ((Map.toList::Map.Map Integer Integer -> [(Integer, Integer)]) (Map.empty))
    ]
  
  fromListTest = testGroup "Test fromList function" [

    QC.testProperty "fromList should behave like Data.Map.fromList (int-int pair version)" $
      \ l -> testOneArgFuncByIso
              (Map.fromList :: [(Integer, Integer)] -> Map.Map Integer Integer)
              ((Data.Map.fromList :: [(Integer, Integer)] -> Data.Map.Map Integer Integer).reverse)
              ((l :: [(Integer, Integer)] ) ),
    
    QC.testProperty "fromList should behave like Data.Map.fromList (int-bool pair version)" $
      \ l -> testOneArgFuncByIso
              (Map.fromList :: [(Integer, Bool)] -> Map.Map Integer Bool)
              ((Data.Map.fromList :: [(Integer, Bool)] -> Data.Map.Map Integer Bool).reverse)
              ((l :: [(Integer, Bool)] ) ),
    
    QC.testProperty "for all list, converting the list to map, the size of the map should equal of smaller to the size of the list" $
      \ l -> Map.size ( Map.fromList (l::[(Integer, Integer)]) ) <= (length l)
    ]

  insertTest = testGroup "Test insert function" [

    QC.testProperty "insert should behave like Data.Map.insert (int-int pair version)" $
      \ k v m -> testOneArgFuncByIso
                  (Map.insert (k::Integer) (v::Integer) :: Map.Map Integer Integer -> Map.Map Integer Integer)
                  (Data.Map.insert k v :: Data.Map.Map Integer Integer -> Data.Map.Map Integer Integer)
                  m,

    QC.testProperty "insert should behave like Data.Map.insert (int-bool pair version)" $
      \ k v m -> testOneArgFuncByIso
                  (Map.insert (k::Integer) (v::Bool) :: Map.Map Integer Bool -> Map.Map Integer Bool)
                  (Data.Map.insert k v :: Data.Map.Map Integer Bool -> Data.Map.Map Integer Bool)
                  m,

    QC.testProperty "what have been inserted should be in the map" $
      \ k v m -> (Map.lookup k $ Map.insert (k::Integer) (v::Bool) m) == (Just v),
    
    QC.testProperty "given a key exists in the map, insert should change the value if the new value not equals to old value" $ 
      \m i-> (i < (length (Map.toList (m::Map.Map Integer Integer)) )) && (m /= Map.empty) && (i>=0) QC.==>
              let
                  l = Map.toList m
                  originKey = fst (l !! i)
                  originValue = snd (l !! i)
              in case (Map.lookup originKey (Map.insert originKey (originValue+1) m)) of
                  Nothing -> False
                  Just n -> originValue + 1 == n
    ]
  
  toListTest = testGroup "Test toList function" [
      QC.testProperty "toList should behave like Data.Map.toList (int-int pair version)" $
        \ m -> testOneArgFuncByIso
                (Map.toList :: Map.Map Integer Integer -> [(Integer, Integer)] )
                (Data.Map.toList :: Data.Map.Map Integer Integer -> [(Integer, Integer)] )
                (m :: Map.Map Integer Integer),

      QC.testProperty "toList should behave like Data.Map.toList (int-bool pair version)" $
        \ m -> testOneArgFuncByIso
                (Map.toList :: Map.Map Integer Bool -> [(Integer, Bool)] )
                (Data.Map.toList :: Data.Map.Map Integer Bool -> [(Integer, Bool)] )
                (m :: Map.Map Integer Bool)
    ]

  sizeTest = testGroup "Test size function" [
      QC.testProperty "size should behave like Data.Map.size (int-int pair version)" $
        \ m -> testOneArgFuncByIso 
                (Map.size :: Map.Map Integer Integer -> Int)
                (Data.Map.size :: Data.Map.Map Integer Integer -> Int)
                m,

      QC.testProperty "size should behave like Data.Map.size (int-bool pair version)" $
        \ m -> testOneArgFuncByIso 
                (Map.size :: Map.Map Integer Bool -> Int)
                (Data.Map.size :: Data.Map.Map Integer Bool -> Int)
                m
    ]
  
  memberTest = testGroup "Test member function" [
      QC.testProperty "member should behave like Data.Map.member (int-int pair version)" $
        \ m i -> testOneArgFuncByIso 
                  (Map.member (i::Integer) :: Map.Map Integer Integer -> Bool)
                  (Data.Map.member i :: Data.Map.Map Integer Integer -> Bool)
                  (m:: Map.Map Integer Integer),
      
      QC.testProperty "member should behave like Data.Map.member (int-bool pair version)" $
        \ m i -> testOneArgFuncByIso 
                  (Map.member (i::Integer) :: Map.Map Integer Bool -> Bool)
                  (Data.Map.member i :: Data.Map.Map Integer Bool -> Bool)
                  (m:: Map.Map Integer Bool),

      QC.testProperty "a specific test for member function: member should return false when i is not in the map" $
        not (Map.member 1 $ Map.fromList [(5,'a'), (3,'b')] ),
      
      QC.testProperty "another specific test for member function: member should return true when i is not in the map" $
        (Map.member 5 $ Map.fromList [(5,'a'), (3,'b')] )
    ]
  
  lookupTest = testGroup "Test lookup function" [
    QC.testProperty "lookup should behave like Data.Map.lookup (int-int pair version)" $
      \ m i -> testOneArgFuncByIso 
                (Map.lookup (i::Integer) :: Map.Map Integer Integer -> Maybe Integer)
                (Data.Map.lookup i :: Data.Map.Map Integer Integer -> Maybe Integer)
                (m:: Map.Map Integer Integer),
            
    QC.testProperty "lookup should behave like Data.Map.lookup (int-bool pair version)" $
      \ m i -> testOneArgFuncByIso 
                (Map.lookup (i::Integer) :: Map.Map Integer Bool -> Maybe Bool)
                (Data.Map.lookup i :: Data.Map.Map Integer Bool -> Maybe Bool)
                (m:: Map.Map Integer Bool),

    QC.testProperty "a specific test for lookup function: lookup should return Nothing when key is not in the map" $
      Nothing == (Map.lookup 1 $ Map.fromList [(5,'a'), (3,'b')] ),

    QC.testProperty "another specific test for lookup function: member should return true when key is in the map" $
      (Just 'a') == (Map.lookup 5 $ Map.fromList [(5,'a'), (3,'b')] )
    ]
  
  deleteTest = testGroup "Test delete function" [
    QC.testProperty "delete should behave like Data.Map.delete (int-int pair version)" $
      \ m i -> testOneArgFuncByIso 
                (Map.delete (i::Integer) :: Map.Map Integer Integer -> Map.Map Integer Integer)
                (Data.Map.delete i :: Data.Map.Map Integer Integer -> Data.Map.Map Integer Integer)
                (m :: Map.Map Integer Integer),
    
    QC.testProperty "delete should behave like Data.Map.delete (int-bool pair version)" $
      \ m i -> testOneArgFuncByIso 
                (Map.delete (i::Integer) :: Map.Map Integer Bool -> Map.Map Integer Bool)
                (Data.Map.delete i :: Data.Map.Map Integer Bool -> Data.Map.Map Integer Bool)
                (m :: Map.Map Integer Bool),

    QC.testProperty "a specific test for delete function: key is in the map" $
      Map.delete 5 (Map.fromList [(5,"a"), (3,"b")]) == Map.fromList [(3, "b")],
    
    QC.testProperty "another specific test for delete function: key is not in the map" $
      Map.delete 7 (Map.fromList [(5,"a"), (3,"b")]) == Map.fromList [(5,"a"), (3,"b")]
    ]
  
  updateF x = if x == "a" then Just "new a" else Nothing
  updatePlusOneIfPositive x = if x > 0 then Just (x+1) else Nothing

  updateTest = testGroup "Test update function" [
    QC.testProperty "update should behave like Data.Map.update (int-int pair version)" $
      \ m i -> testOneArgFuncByIso 
                (Map.update updatePlusOneIfPositive (i::Integer) :: Map.Map Integer Integer -> Map.Map Integer Integer)
                (Data.Map.update updatePlusOneIfPositive i :: Data.Map.Map Integer Integer -> Data.Map.Map Integer Integer)
                (m :: Map.Map Integer Integer),
        
    QC.testProperty "a specific test for update function: key is in the map" $
      (Map.update updateF 5 (Map.fromList [(5,"a"), (3,"b")] )) == Map.fromList [(3, "b"), (5, "new a")],

    QC.testProperty "another specific test for update function: key is not in the map" $
      (Map.update updateF 7 (Map.fromList [(5,"a"), (3,"b")] )) == Map.fromList [(3, "b"), (5, "a")]
    ]

  unionTest = testGroup "Test union function" [
    QC.testProperty "union should behave like Data.Map.union (int-int pair version)" $
      \ m1 m2 -> testTwoArgFuncByIso 
                  (Map.union :: Map.Map Integer Integer -> Map.Map Integer Integer -> Map.Map Integer Integer)
                  (Data.Map.union :: Data.Map.Map Integer Integer -> Data.Map.Map Integer Integer -> Data.Map.Map Integer Integer)
                  (m1 :: Map.Map Integer Integer)
                  (m2 :: Map.Map Integer Integer),
            
    QC.testProperty "union should behave like Data.Map.union (int-bool pair version)" $
      \ m1 m2 -> testTwoArgFuncByIso 
                  (Map.union :: Map.Map Integer Bool -> Map.Map Integer Bool -> Map.Map Integer Bool)
                  (Data.Map.union :: Data.Map.Map Integer Bool -> Data.Map.Map Integer Bool -> Data.Map.Map Integer Bool)
                  (m1 :: Map.Map Integer Bool)
                  (m2 :: Map.Map Integer Bool),
                    
    QC.testProperty "a specific test for union: should have some update" $
      Map.union (Map.fromList [(5, "a"), (3, "b")]) (Map.fromList [(5, "A"), (7, "C")]) == Map.fromList [(3, "b"), (5, "a"), (7, "C")]
    ]

  filterTest = testGroup "Test filter function" [
    QC.testProperty "filter should behave like Data.Map.filter (int-int pair version)" $
      \ m -> testOneArgFuncByIso 
              (Map.filter (>1) :: Map.Map Integer Integer -> Map.Map Integer Integer)
              (Data.Map.filter (>1) :: Data.Map.Map Integer Integer -> Data.Map.Map Integer Integer)
              (m :: Map.Map Integer Integer),
      
    QC.testProperty "filter should behave like Data.Map.filter (int-bool pair version)" $
      \ m -> testOneArgFuncByIso 
              (Map.filter (\x->x) :: Map.Map Integer Bool -> Map.Map Integer Bool)
              (Data.Map.filter (\x->x) :: Data.Map.Map Integer Bool -> Data.Map.Map Integer Bool)
              (m :: Map.Map Integer Bool),
    
    QC.testProperty "a specific test for filter: should return empty" $
      Map.filter (> "x") (Map.fromList [(5,"a"), (3,"b")]) == Map.empty,
    
    QC.testProperty "another specific test for filter: should return something" $
      Map.filter (> "a") (Map.fromList [(5,"a"), (3,"b")]) == Map.fromList [(3, "b")]
    ]
  
  equalTest = testGroup "Test equal function" [
    QC.testProperty "same map (int-int pair version) should equal" $
      \ m -> m == (m :: Map.Map Integer Integer),

    QC.testProperty "same map (int-bool pair version) should equal" $
      \ m -> m == (m :: Map.Map Integer Bool),

    QC.testProperty "map equal should not relevant to the order of the list (HINT: MAP'S EQUIVALENCE IS NOT RELATED TO ITS STRUCTURE)" $
      Map.fromList [(5,"a"), (3,"b"), (1, "c")] == Map.fromList [(3,"b"), (5,"a"), (1, "c")]
    ]
