module ExampleTest where 

  import TestBase
  import Test.Tasty.QuickCheck as QC
  import Test.Tasty (defaultMain, testGroup)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

  exampleTest = testGroup "test all example is defined" [
    testCase "example of `DayOfTheWeek` should have been defined and teminating" $
    assertEqual [] (example :: DayOfTheWeek) (example :: DayOfTheWeek),

    testCase "example of `Integer` should have been defined and teminating" $
    assertEqual [] (example :: Integer) (example :: Integer),

    testCase "example of `Bool` should have been defined and teminating" $
    assertEqual [] (example :: Bool) (example :: Bool),

    testCase "example of `[a]` should have been defined and teminating" $
    assertEqual [] (example :: [Int]) (example :: [Int]),

    testCase "example of `(Bool, (Integer, [a]))` should have been defined and teminating" $
    assertEqual [] (example :: (Bool, (Integer, [Int]))) example,

    testCase "example of `(Integer, (Integer, [a]))` should have been defined and teminating" $
    assertEqual [] (example :: (Integer, (Integer, [Int]))) example,

    QC.testProperty "example of `Integer -> Bool` should have been defined and teminating" $
    \n -> ((example :: Integer -> Bool) n) == example n,

    QC.testProperty "example of `Silly -> Bool` (Silly is defined in test) should have been defined and teminating" $
    \n -> ((example :: Silly -> Bool) n) == example n
    
    ]