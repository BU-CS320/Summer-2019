module IsValueTest where
  
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

  import LambdaCalcImplementation (isValue)
  import Examples (isValRes, Res(..))
  
  foldTestCase [] = return ()
  foldTestCase (test1:testRest) = 
    do 
      test1
      foldTestCase testRest

  isValueTest = testCase "isValue test" $ foldTestCase $
    [assertEqual testStr res (isValue formula) | (Res testStr formula res) <- isValRes]
