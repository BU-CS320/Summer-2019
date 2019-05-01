module IsClosedTest where
  
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
  import LambdaCalcImplementation (isClosed)
  import Examples (isClosedRes, Res(..))

  foldTestCase [] = return ()
  foldTestCase (test1:testRest) = 
    do 
      test1
      foldTestCase testRest

  isClosedTest = testCase "isClosed test"  $ foldTestCase $
    [assertEqual testStr res (isClosed formula) | (Res testStr formula res) <- isClosedRes]
