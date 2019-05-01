module FreeVarsTest where
  
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
  import LambdaCalcImplementation (freeVars)
  import Examples (freeVarsRes, Res(..))
  
  foldTestCase [] = return ()
  foldTestCase (test1:testRest) = 
    do 
      test1
      foldTestCase testRest

  freeVarsTest = testCase "freeVars test" $ foldTestCase $
    [assertEqual testStr res (freeVars formula) | (Res testStr formula res) <- freeVarsRes]