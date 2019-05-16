{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where


--TODO: note for next year: 
-- clean up into separate files, remove the non standard indentation
-- use typeclasses for the conversion, and to give definitions of equality to existing data, so conversions are not needed
-- quick check become very counter productive


  import Hw
  import Data.Tree
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  
  main = defaultMain allTests
  
  allTests = testGroup "allTests" 
    [
      boolTest
      , natTest
      , dayTest
      , pointTest
      , shortAnsTest
      , natListTest
      , listTest
      , treeTests
    ]
  
  -- Utils 
  -- 

  -- This class defines an isomorphism between two types,
  -- we use this to link types in Hw to types in Prelude to make it easier to test
  class Iso a b where
    isoMap :: a -> b  -- isoMap will map HW to Prelude
    isoInverse :: b -> a  -- isoInverse is the inverse of isMap
    
  -- define the isomorphism between Hw.Bool and Prelude.Bool
  instance Iso Hw.Bool Prelude.Bool where 
    isoMap Hw.True = Prelude.True 
    isoMap Hw.False = Prelude.False
    isoInverse Prelude.True =  Hw.True 
    isoInverse Prelude.False = Hw.False
  
  -- define the iso between Nat and a subset of integer
  instance Iso Hw.Nat Prelude.Integer where 
    isoMap Zero = 0
    isoMap (Succ n) = (isoMap n) + 1
    isoInverse 0 = Zero 
    isoInverse n 
      | n < 0 = undefined -- don't convert nagative number to Nat
      | n > 0 = Succ (isoInverse (n - 1)) 

  -- define the iso between Nat and a subset of int
  instance Iso Hw.Nat Prelude.Int where 
    isoMap Zero = 0
    isoMap (Succ n) = (isoMap n) + 1
    isoInverse 0 = Zero 
    isoInverse n 
      | n < 0 = undefined -- don't convert nagative number to Nat
      | n > 0 = Succ (isoInverse (n - 1)) 

  -- define the iso between Hw point and prelude point
  instance Iso Hw.Point (Integer, Integer) where 
    isoMap p = (isoMap . getX $ p, isoMap . getY $ p)
    isoInverse (x, y) = makePoint (isoInverse x) (isoInverse y)

  -- define the iso between ListNat and List Integer 
  instance Iso ListNat [Integer] where 
    isoMap NilNat = []
    isoMap (ConsNat h t) = (isoMap h):(isoMap t) 
    isoInverse [] = NilNat
    isoInverse (h:t) = ConsNat (isoInverse h) (isoInverse t)

  instance (Iso a b) => Iso (List a) [b] where
    isoMap Nil = [] 
    isoMap (Cons h t) = (isoMap h):(isoMap t) 
    isoInverse [] = Nil 
    isoInverse (h:t) = Cons (isoInverse h) (isoInverse t)
  
  -- this is a correction of the native haskell tree
  -- it includes a tree and a value that represent 
  -- a empty tree
  -- because there is no empty tree in haskell tree type
  data TreeWrapper a = Wrap (Data.Tree.Tree a) a deriving Show

  -- get the value in the null tree
  nullTreeVal (Wrap t v) = v 
  -- unwrap the tree
  unwrap (Wrap t e) = t
  
  -- define the isomorphism between non-empty homework tree and binary haskell tree
  -- since haskell tree only defined on the non-empty case
  -- and hw tree only defines the binary case  
  -- for the empty tree it is
  -- Wrap (Data.Tree.Node (toEnum 0) []) (toEnum 0)
  instance (Iso a b, Enum b, Eq b) => Iso (Hw.Tree a) (TreeWrapper b) where 
    isoMap Null = Wrap (Data.Tree.Node (toEnum 0) []) (toEnum 0)
    isoMap (Hw.Node ltree value rtree) = 
      Wrap 
        (Data.Tree.Node (isoMap value) [unwrap . isoMap $ ltree,
                                        unwrap . isoMap $ rtree])
        (toEnum 0)

    isoInverse (Wrap (Data.Tree.Node value []) e) 
      | value == e = Hw.Null
      | otherwise = undefined
    isoInverse (Wrap (Data.Tree.Node value [ltree, rtree]) e) = 
      Hw.Node (isoInverse . Wrap ltree $ e) (isoInverse value) (isoInverse . Wrap rtree $ e)
    isoInverse _ = undefined
      

  -- this function test a function by existing isomorphic function
  -- it feeds the testing function and the existing function with isomorphic parameters
  -- and their return should be also isomorphic:
  -- idea: if A isomorphic to B, and f: A -> A is isomorphic to g: B -> B
  --       then f(a) needs to be isomorphic to g(b)
  testOneArgFuncByIso :: 
    (Iso aI bI, Iso aO bO, Eq bO, Show bI, Show bO) => 
    String ->  -- the message for success
    (aI -> aO) ->  -- the function you implemented
    (bI -> bO) ->  -- the existing function that is isomorphic to your function
    aI ->  -- the input for the function you want to test
    TestTree 
  testOneArgFuncByIso sucMsg f g input = testCase sucMsg $ 
    assertEqual [] (g . isoMap $ input) (isoMap . f $ input)

  testTwoArgFuncByIso :: 
    (Iso aI1 bI1, Iso aI2 bI2, Iso aO bO, Eq bO, Show bO, Show aO, Show aI1, Show aI2) => 
    String ->  -- the message for success
    (aI1 -> aI2 -> aO) ->  -- the function you implemented
    (bI1 -> bI2 -> bO) ->  -- the existing function that is isomorphic to your function
    aI1 -> aI2 ->  -- the input for the function you want to test
    TestTree
  testTwoArgFuncByIso sucMsg f g input1 input2 = testCase sucMsg $ 
    assertEqual [] (g (isoMap input1) (isoMap input2)) (isoMap (f input1 input2))

  -- this function converts from our custom Bools to the standard Bools so we have a lot of automatic things already defined
  fromhwBoolToStandardBool :: Hw.Bool -> Prelude.Bool
  fromhwBoolToStandardBool Hw.True  = Prelude.True
  fromhwBoolToStandardBool Hw.False = Prelude.False

  -- this function converts from our custom Bools to the standard Bools so we have a lot of automatic things already defined
  fromStdBoolToHwBool :: Prelude.Bool -> Hw.Bool
  fromStdBoolToHwBool Prelude.True  = Hw.True
  fromStdBoolToHwBool Prelude.False = Hw.False
  
  -- this function converts from our custom Nats to the standard Integers so we have a lot of automatic things already defined
  fromNatToInteger :: Nat -> Integer
  fromNatToInteger Zero     = 0
  fromNatToInteger (Succ n) = 1 + (fromNatToInteger n)
  
  
  fromIntegerToNat :: Integer -> Nat
  fromIntegerToNat 0     = Zero
  fromIntegerToNat x = (Succ(fromIntegerToNat (x-1)))
  
  -- this function converts from our List to the standard List so we have a lot of automatic things already defined
  fromhwListToStandardList :: Hw.List a -> [a]
  fromhwListToStandardList Nil      = []
  fromhwListToStandardList (Cons x y) = x:fromhwListToStandardList y
  
  fromStandardListToHwList :: [a] -> Hw.List a 
  fromStandardListToHwList []     = Nil 
  fromStandardListToHwList ( x :  y) = Cons x  (fromStandardListToHwList y)
  
  
  fromtandardList2HwLsNat :: [Integer] -> Hw.ListNat
  fromtandardList2HwLsNat []      = NilNat
  fromtandardList2HwLsNat (x: y) = (ConsNat (fromIntegerToNat x) (fromtandardList2HwLsNat y))
  
  fromtandardHwLsNat2Ls :: Hw.ListNat -> [Integer]
  fromtandardHwLsNat2Ls NilNat      = []
  fromtandardHwLsNat2Ls (ConsNat x y) = (fromNatToInteger x ) :(fromtandardHwLsNat2Ls y)
  
  
  instance Eq Hw.Bool where
    Hw.True == Hw.True  = Prelude.True
    Hw.True == _  = Prelude.False
    Hw.False == Hw.False  = Prelude.True
    Hw.False == _  = Prelude.False
    
  instance Eq Hw.Nat where
    Hw.Zero == Hw.Zero  = Prelude.True
    (Succ x) == (Succ y)  = x == y
    _ == _  = Prelude.False
    
    
  instance  (Eq a) => Eq (Hw.List a) where
    Hw.Nil == Hw.Nil  = Prelude.True
    (Hw.Cons x xs) == (Hw.Cons y ys)  = (x == y) && (xs == ys)
    _ == _  = Prelude.False
  
  
  ------- Tests 
  
  ---- Test for Bool ----
  boolTest = testGroup "test for all the boolean functions"
    [
      notTest,
      andTest,
      orTest,
      xorTest
    ]
  
  -- not test
  notTest = testGroup "test not function" 
    [
      testOneArgFuncByIso ("testing not " ++ show input) 
       Hw.not Prelude.not input | input <- [Hw.True, Hw.False]
    ]

  -- and test
  correctAnd b1 b2 = fromStdBoolToHwBool (fromhwBoolToStandardBool b1 && fromhwBoolToStandardBool b2)
  andTest = testGroup "test and function"
    [
      testTwoArgFuncByIso ("testing and " ++ show b1 ++ " " ++ show b2) 
        Hw.and (&&) b1 b2
      | b1 <- [Hw.True, Hw.False], b2 <- [Hw.True, Hw.False]
    ]

  -- or test
  orTest = testGroup "test or function"
    [
      testTwoArgFuncByIso ("testing or " ++ show b1 ++ " " ++ show b2) 
        Hw.or (||) b1 b2
      | b1 <- [Hw.True, Hw.False], b2 <- [Hw.True, Hw.False]
    ]
  
  -- xor test
  xorTemp b1 b2 = (b1 || b2) && Prelude.not (b1 && b2)
  xorTest = testGroup "test xor function"
    [
      testTwoArgFuncByIso ("testing xor " ++ show b1 ++ " " ++ show b2) 
        Hw.xor xorTemp b1 b2
      | b1 <- [Hw.True, Hw.False], b2 <- [Hw.True, Hw.False]
    ]
  
  ---- Test for Nat ----
  natTest = testGroup "test for all nat function" 
    [
      basicNatTest,
      addTest,
      multTest,
      expTest,
      eqTest,
      neTest,
      ltTest,
      gtTest,
      leTest,
      geTest,
      isEvenTest,
      maxTest
    ]

  -- basic nat
  basicNatTest = testGroup "test basic nat definition"
    [
      testCase "test zero" $ assertEqual [] 0 (isoMap (Hw.zero::Hw.Nat)::Integer),
      testCase "test one" $ assertEqual [] 1 (isoMap (Hw.one::Hw.Nat)::Integer),
      testCase "test two" $ assertEqual [] 2 (isoMap (Hw.two::Hw.Nat)::Integer),
      testCase "test three" $ assertEqual [] 3 (isoMap (Hw.three::Hw.Nat)::Integer),
      testCase "test four" $ assertEqual [] 4 (isoMap (Hw.four::Hw.Nat)::Integer),
      testCase "test five" $ assertEqual [] 5 (isoMap (Hw.five::Hw.Nat)::Integer)      
    ]
  
  -- test addition
  addTest = testGroup "test add function"
    [
      testTwoArgFuncByIso  ("testing add " ++ show a1 ++ " " ++ show a2) 
        Hw.add ((+)::Integer -> Integer -> Integer) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]


  -- test mult
  multTest = testGroup "test mult function"
    [
      testTwoArgFuncByIso ("testing mult " ++ show a1 ++ " " ++ show a2) 
        Hw.mult ((*)::Integer -> Integer -> Integer) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]
  
  -- test exponential
  expTemp x y = (^) x y
  expTest = testGroup "test exp function"
    [
      testTwoArgFuncByIso ("testing exp " ++ show a1 ++ " " ++ show a2) 
        Hw.exp (expTemp::Integer -> Integer -> Integer) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]

  -- test equality
  eqTest = testGroup "test eq function"
    [
      testTwoArgFuncByIso ("testing eq " ++ show a1 ++ " " ++ show a2) 
        Hw.eq ((==)::Integer -> Integer -> Prelude.Bool) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]

  -- test inequality
  neTest = testGroup "test ne function"
    [
      testTwoArgFuncByIso ("testing ne " ++ show a1 ++ " " ++ show a2) 
        Hw.ne ((/=)::Integer -> Integer -> Prelude.Bool) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]

  -- test less than
  ltTest = testGroup "test lt function"
    [
      testTwoArgFuncByIso ("testing lt " ++ show a1 ++ " " ++ show a2) 
        Hw.lt ((<)::Integer -> Integer -> Prelude.Bool) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]

  -- test greater than
  gtTest = testGroup "test gt function"
    [
      testTwoArgFuncByIso ("testing gt " ++ show a1 ++ " " ++ show a2) 
        Hw.gt ((>)::Integer -> Integer -> Prelude.Bool) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]
  
  -- test less than equal
  leTest = testGroup "test le function"
    [
      testTwoArgFuncByIso ("testing le " ++ show a1 ++ " " ++ show a2) 
        Hw.le ((<=)::Integer -> Integer -> Prelude.Bool) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]

  -- test greater than equal
  geTest = testGroup "test ge function"
    [
      testTwoArgFuncByIso ("testing ge " ++ show a1 ++ " " ++ show a2) 
        Hw.ge ((>=)::Integer -> Integer -> Prelude.Bool) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]
  
  -- test even
  tempEven s = (s `mod` 2) == 0
  isEvenTest = testGroup "test isEven function"
    [
      testOneArgFuncByIso ("testing isEven " ++ show a1) 
        Hw.isEven (tempEven :: Integer -> Prelude.Bool)
        ((isoInverse::Integer -> Nat) a1)
      | a1 <- [0 .. 5]
    ]

  -- test max
  maxTemp x y 
    | x < y = y 
    | otherwise = x 
  maxTest = testGroup "test ge function"
    [
      testTwoArgFuncByIso ("testing max " ++ show a1 ++ " " ++ show a2) 
        Hw.max (maxTemp::Integer -> Integer -> Integer) 
        ((isoInverse::Integer -> Nat) a1)
        ((isoInverse::Integer -> Nat) a2)  
      | a1 <- [0 .. 5], a2 <- [0 .. 5]
    ]
  
  ---- Test for Days ----
  -- implement Eq type class for DaysOfWeek
  deriving instance Eq DayOfWeek

  -- this is a helper function to calculate the day for next n days
  nextNDaysOf :: DayOfWeek -> Int -> DayOfWeek
  nextNDaysOf d n
    | n < 0 = undefined  -- do not support previous days
    | n == 0 = d 
    | n > 0 = nextNDaysOf (nextDay d) (n - 1)
  
  -- a list of all the posible days in week
  allPossibleDays :: [DayOfWeek]
  allPossibleDays = map (nextNDaysOf favoriteDay) [0..6]

  weekends :: [DayOfWeek]
  weekends = filter (isoMap . isWeekend) allPossibleDays

  -- the test for days
  dayTest = testGroup "testing all the function of DayOfWeek"
    [
      dayEqualityTest
      , weekendsTest
    ]

  -- all the days should be different from each other 
  dayEqualityTest = testGroup "test the equality of days"
    [
      testCase ("test equality of the " ++ 
        show n1 ++ "th day after favoriteDay and the " ++ 
        show n2 ++ "th day after favoriteDay")
      $ assertEqual [] (n1 == n2) ((nextNDaysOf d n1) == (nextNDaysOf d n2)) 
      | n1 <- [0..6], n2 <- [0..6]
    ]
    where 
      d = favoriteDay
  
  -- There can be only two weekend
  weekendsTest = testGroup "test for weekends"
      [
        testCase ("There should be " ++ show numberOfWeekends ++ " weekends for " ++ show daysAfterWeekends ++ " days after all the weekends")
        $ assertEqual [] numberOfWeekends 
          (Prelude.length . filter  (isoMap . isWeekend) . map (\d -> nextNDaysOf d daysAfterWeekends) $ weekends)
        | (numberOfWeekends, daysAfterWeekends) <-
            [(2, 0), (1, 1), (0, 2), (0, 3), (0, 4), (0, 5), (1, 6)]
      ]

  ---- Test Point ----
  -- implement Eq type class for Point
  deriving instance Eq Point

  -- test for point
  pointTest = testGroup "test all the function on Points" 
    [
      testUnpackThenPack
      , testPackThenUnpack
      , testManhattanDistance
    ]

  -- test for iso
  testUnpackThenPack = testGroup "test packing and then unpack"
      [
        testCase ("packing with " ++ show n1 ++ " and " ++ show n2 ++ " then unpack")
          $ assertEqual [] (n1, n2) 
            (isoMap . (isoInverse::(Integer, Integer) -> Point) $ (n1, n2))
        | n1 <- [0..2], n2 <- [0..2]
      ]

  testPackThenUnpack = testGroup "test unpack and then pack"
      [
        testCase ("unpacking ("++ show n1 ++ ", " ++ show n2 ++ ") then unpack")
          $ assertEqual [] (makePoint (isoInverse n1) (isoInverse n2))
            ((isoInverse::(Integer, Integer) -> Point) . isoMap $ (makePoint (isoInverse n1) (isoInverse n2)))
        | n1 <- [0..2] :: [Integer], n2 <- [0..2] :: [Integer]
      ]
  
  -- test for manhattanDistance
  manhattanTemp :: (Integer, Integer) -> (Integer, Integer) -> Integer
  manhattanTemp (a1, b1) (a2, b2)= abs (a1 - a2) + abs (b1 - b2)
  testManhattanDistance = testGroup "test for manhattan distance"
    [
      testTwoArgFuncByIso ("testing manhattanDistance between " ++ show (a1, b1) ++ " and " ++ show (a2, b2)) 
        manhattanDistance manhattanTemp 
        ((isoInverse::(Integer, Integer) -> Point) (a1, b1))
        ((isoInverse::(Integer, Integer) -> Point) (a2, b2))  
      | a1 <- [0..2], a2 <- [0..2], b1 <- [0..2], b2 <- [0..2]
    ]
  
  ---- Test for ShortAnswer ----

  shortAnsTest = testGroup "test for short answer"
    [
      ans1Test
      , ansTrueTest
    ]

  -- test for ans1
  ans1Test = testCase "check ans1 is 1" $ assertEqual [] 
    Hw.True (gradeAnswer (answerNat Hw.one) ans1)

  -- test for ansTrue 
  ansTrueTest = testCase "check ansTrue is True" $ assertEqual [] 
    Hw.True (gradeAnswer (answerNat Hw.one) ans1)


  ---- Test for ListNat ----

  -- test sample
  natListTestSample = [
      [],
      [1, 2],
      [3, 1, 4],
      [0, 5, 6],
      [1],
      [1,2,3,4,5,6,7]
    ]::[[Integer]]

  -- test for list nat
  natListTest = testGroup "test all the functions defined on ListNat"
      [
        natLengthTest
        , sumTest
        , eqListTest
        , memberTest
      ]
  
  -- test for length 
  natLengthTest = testGroup "test lengthOfListNat function"
    [
      testOneArgFuncByIso ("testing lengthOfListNat " ++ show l) 
        lengthOfListNat (Prelude.length::[Integer] -> Int) 
        (isoInverse l)
      | l <- natListTestSample
    ]

  -- test for sum 
  sumTest = testGroup "test sum function"
    [
      testOneArgFuncByIso ("testing sum of " ++ show l) 
        Hw.sum (Prelude.sum::[Integer] -> Integer) 
        (isoInverse l)
      | l <- natListTestSample
    ]

  -- test for eqList
  eqListTest = testGroup "test eqList function"
    [
      testTwoArgFuncByIso ("testing equality of " ++ show l1 ++ " and " ++ show l2) 
        Hw.eqList ((==)::[Integer] -> [Integer] -> Prelude.Bool) 
        (isoInverse l1)
        (isoInverse l2)
      | l1 <- natListTestSample, l2 <- natListTestSample
    ]

  -- test for member
  memberTest = testGroup "test eqList function"
    [
      testTwoArgFuncByIso ("testing member " ++ show n ++ " with list " ++ show l) 
        Hw.member (elem::Integer -> [Integer] -> Prelude.Bool) 
        (isoInverse n)
        (isoInverse l)
      | n <- [0, 1, 4]::[Integer], l <- natListTestSample
    ]


  ---- Test for List ---
  -- test sample
  boolListTestSample = [
      [],
      [Prelude.True],
      [Prelude.False],
      [Prelude.True, Prelude.False],
      [Prelude.True, Prelude.True, Prelude.False, Prelude.False]
    ]
  
  -- all the test for List
  listTest = testGroup "all the function on list"
      [
        lengthTestNat
        , lengthTestBool
      ]
  
  -- length test 
  lengthTestNat = testGroup "test length function for List Nat"
    [
      testOneArgFuncByIso ("testing length for list " ++ show l) 
        (Hw.length :: List Nat -> Nat)
        (Prelude.length::[Integer] -> Int) 
        (isoInverse l)
      | l <- natListTestSample
    ]
  lengthTestBool = testGroup "test length function for List Bool"
    [
      testOneArgFuncByIso ("testing length for list " ++ show l) 
        (Hw.length :: List Hw.Bool -> Nat) 
        (Prelude.length::[Prelude.Bool] -> Int) 
        (isoInverse l)
      | l <- boolListTestSample
    ]

  ---- Tests for tree ----

  -- test sample for nonempty-tree
  nonemptyNatTreeTestSample = [
      Hw.Node Null Hw.one Null,
      Hw.Node (Hw.Node (Hw.Node (Hw.Node Null Hw.one Null) Hw.one Null) Hw.one Null) Hw.one Null,
      Hw.Node 
        (Hw.Node (Hw.Node Null Hw.zero Null) Hw.one (Hw.Node Null Hw.three Null)) 
        Hw.two
        (Hw.Node (Hw.Node Null Hw.four Null) Hw.one (Hw.Node Null Hw.five Null)) 
    ] :: [Hw.Tree Nat]
  nonemptyBoolTreeTestSample = [
      Hw.Node Null Hw.True (Hw.Node Null Hw.False (Hw.Node Null Hw.True Null)),   
      Hw.Node Null Hw.False Null
    ] :: [Hw.Tree Hw.Bool]

  -- all test all trees
  treeTests = testGroup "test for Tree type"
    [
      sizeTests,
      heightTests,
      inorderTests,
      preorderTests
    ]

  -- test for size 
  sizeFoldFunc n [] = 0
  sizeFoldFunc n [ltreeRes, rtreeRes] = ltreeRes + rtreeRes + 1
  sizeFoldFunc _ _ = undefined

  sizeCorrect :: (Eq a) => TreeWrapper a -> Int
  sizeCorrect (Wrap t e) = foldTree sizeFoldFunc t

  sizeEmptyTest = 
    testCase "empty tree should have size 0"
    $ assertEqual [] Hw.Zero (Hw.size Null)
  sizeNonEmptyNatTest = [
      testOneArgFuncByIso "testing size of a non-empty nat tree"
        Hw.size (sizeCorrect:: TreeWrapper Integer -> Int)
        natTree
      | natTree <- nonemptyNatTreeTestSample
    ]
  sizeNonEmptyBoolTest = [
      testOneArgFuncByIso "testing size of a non-empty bool tree"
        Hw.size (sizeCorrect:: TreeWrapper Prelude.Bool -> Int)
        boolTree
      | boolTree <- nonemptyBoolTreeTestSample
    ]
  sizeTests = testGroup "testing the size function"
    (sizeEmptyTest : sizeNonEmptyNatTest ++ sizeNonEmptyBoolTest)

  -- test for height
  heightFoldFunc n [] = 0
  heightFoldFunc n [ltreeRes, rtreeRes] = Prelude.max ltreeRes rtreeRes + 1
  heightFoldFunc _ _ = undefined

  heightCorrect :: (Eq a) => TreeWrapper a -> Int
  heightCorrect (Wrap t e) = foldTree heightFoldFunc t

  heightEmptyTest = testCase "empty tree should have height 0"
    $ assertEqual [] Hw.Zero (Hw.height Null)

  heightNonEmptyNatTest = [
      testOneArgFuncByIso "testing height of a non-empty nat tree"
        Hw.height (heightCorrect :: TreeWrapper Integer -> Int)
        natTree
      | natTree <- nonemptyNatTreeTestSample
    ]

  heightNonEmptyBoolTest = [
      testOneArgFuncByIso "testing height of a non-empty bool tree"
        Hw.height (heightCorrect :: TreeWrapper Prelude.Bool -> Int)
        boolTree
      | boolTree <- nonemptyBoolTreeTestSample
    ]

  heightTests = testGroup "testing the height function"
    (heightEmptyTest : heightNonEmptyNatTest ++ heightNonEmptyBoolTest)


  -- test for inorder traversal 
  inOrderFoldFunc n [] = []
  inOrderFoldFunc n [ltreeRes, rtreeRes] = ltreeRes ++ [n] ++ rtreeRes
  inOrderFoldFunc _ _ = undefined

  inOrderCorrect :: (Eq a) => TreeWrapper a -> [a]
  inOrderCorrect (Wrap t e) = foldTree inOrderFoldFunc t

  inOrderEmptyTest = testCase "in order traverse empty tree should return []"
    $ assertEqual [] Nil (Hw.inorder Null::List Integer)

  inOrderNonEmptyNatTest = [
      testOneArgFuncByIso "testing inorder of a non-empty nat tree"
        Hw.inorder (inOrderCorrect :: TreeWrapper Integer -> [Integer])
        natTree
      | natTree <- nonemptyNatTreeTestSample
    ]

  inOrderNonEmptyBoolTest = [
      testOneArgFuncByIso "testing inorder of a non-empty bool tree"
        Hw.inorder (inOrderCorrect :: TreeWrapper Prelude.Bool -> [Prelude.Bool])
        boolTree
      | boolTree <- nonemptyBoolTreeTestSample
    ]

  inorderTests = testGroup "testing the inorder function"
    (inOrderEmptyTest : inOrderNonEmptyNatTest ++ inOrderNonEmptyBoolTest)

  -- test for preorder traversal 
  preOrderFoldFunc n [] = []
  preOrderFoldFunc n [ltreeRes, rtreeRes] = [n] ++ ltreeRes ++ rtreeRes
  preOrderFoldFunc _ _ = undefined

  preOrderCorrect :: (Eq a) => TreeWrapper a -> [a]
  preOrderCorrect (Wrap t e) = foldTree preOrderFoldFunc t

  preOrderEmptyTest = testCase "preorder traverse empty tree should return []"
    $ assertEqual [] Nil (Hw.inorder Null::List Integer)

  preOrderNonEmptyNatTest = [
      testOneArgFuncByIso "testing preorder of a non-empty nat tree"
        Hw.preorder (preOrderCorrect :: TreeWrapper Integer -> [Integer])
        natTree
      | natTree <- nonemptyNatTreeTestSample
    ]

  preOrderNonEmptyBoolTest = [
      testOneArgFuncByIso "testing preorder of a non-empty bool tree"
        Hw.preorder (preOrderCorrect :: TreeWrapper Prelude.Bool -> [Prelude.Bool])
        boolTree
      | boolTree <- nonemptyBoolTreeTestSample
    ]

  preorderTests = testGroup "testing the preorder function"
    (preOrderEmptyTest : preOrderNonEmptyNatTest ++ preOrderNonEmptyBoolTest)
