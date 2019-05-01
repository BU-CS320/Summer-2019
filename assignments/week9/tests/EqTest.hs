module EqTest where
  
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
  import LambdaCalcImplementation (Term)
  import Examples (eqRes, ResTwo(..))
  
  foldTestCase [] = return ()
  foldTestCase (test1:testRest) = 
    do 
      test1
      foldTestCase testRest

  eqTest = testCase "eq test" $ foldTestCase $
    [assertEqual ("equality for " ++ testStrL ++ " and " ++ testStrR) res (l == r) | (ResTwo (testStrL, testStrR) (l, r) res) <- eqRes]