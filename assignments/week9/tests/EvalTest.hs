module EvalTest where
  
  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

  import LambdaCalcImplementation (eval)
  import Examples (evalRes, Res(..))
  
  foldTestCase [] = return ()
  foldTestCase (test1:testRest) = 
    do 
      test1
      foldTestCase testRest

  evalTest = testCase "eval test" $ foldTestCase $
    [assertEqual testStr res (eval formula) | (Res testStr formula res) <- evalRes]
