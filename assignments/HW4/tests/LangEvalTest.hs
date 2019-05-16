{-# LANGUAGE ScopedTypeVariables #-}
module LangEvalTest where

  import Test.Tasty (testGroup)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
  import Test.Tasty.QuickCheck hiding (Fun)
  import qualified Test.Tasty.QuickCheck as QC (Fun(..))
  
  import qualified Data.Map as Map
  
  import Lang (eval, Ast(..), Env, Val(..), run)
  import EnvUnsafe (runEnvUnsafe, Unsafe(..), EnvUnsafe(..))

  -- | this is the val used for testing,
  -- we don't include function because it is not Equaliable
  data TestVal = TI Integer | TB Bool
        | TLs [TestVal] deriving (Eq, Show)

  -- | an unsafe function to convert Val to TestVal
  valToTestVal :: Val -> TestVal 
  valToTestVal (I n) = TI n 
  valToTestVal (B b) = TB b 
  valToTestVal (Ls lst) = TLs $ map valToTestVal lst 
  valToTestVal (Lang.Fun _) = error "unexpected function construction"

  -- | Test if two Unsafe Int val is equal 
  eqInt :: Unsafe Val -> Unsafe Val -> Bool 
  eqInt (Ok (I n)) (Ok (I m)) = m == n 
  eqInt _ _ = False 

  -- | Test if two Unsafe Boolean is equal 
  eqBool :: Unsafe Val -> Unsafe Val -> Bool 
  eqBool (Ok (B b1)) (Ok (B b2)) = b1 == b2 
  eqBool _ _ = False

  -- | Apply a unsafe function
  appFunc :: Unsafe Val -> Unsafe Val -> Unsafe Val 
  appFunc (Error msg) _ = (Error msg)
  appFunc _ (Error msg) = (Error msg)
  appFunc (Ok (Fun f)) (Ok val) = f val 
  appFunc (Ok val) _ = Error "first input is not a function in appFunc"

  -- | test if an unsafe is error
  isError :: Unsafe a -> Bool 
  isError (Error _) = True 
  isError _ = False 

  -- | return if the two unsafe has the same error
  sameError :: Unsafe a -> Unsafe b -> Bool 
  sameError (Error msg1) (Error msg2) = msg1 == msg2
  sameError _ _ = False

  -- | run a program under empty exprssion
  runEmpty :: Ast -> Unsafe Val 
  runEmpty exp = runEnvUnsafe (eval exp) Map.empty
  runEmptyT :: Ast -> Unsafe TestVal 
  runEmptyT exp = 
    case runEnvUnsafe (eval exp) Map.empty of 
      Ok res -> Ok $ valToTestVal res 
      Error msg -> Error msg

  -- | Get the subexpression of the ast 
  subexpression :: Ast -> [Ast] 
  subexpression (ValBool _) = []
  subexpression (And r l) = [r, l]
  subexpression (Or r l) = [r, l]
  subexpression (Not inp) = [inp]
  subexpression (ValInt _) = []
  subexpression (Plus r l) = [r, l]
  subexpression (Minus r l) = [r, l]
  subexpression (Mult r l) = [r, l]
  subexpression (Div r l) = [r, l]
  subexpression (Nil) = []
  subexpression (Cons r l) = [r, l]
  subexpression (If b trueExp falseExp) = [b, trueExp, falseExp]
  subexpression (Let varName varExp bodyExp) = [varExp, bodyExp]
  subexpression (Var _) = []
  subexpression (Lam para body) = [body]
  subexpression (App f x) = [f, x]

  instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

  arbitrarySizedAst ::  Int -> Gen Ast
  arbitrarySizedAst m | m < 1 = 
    do 
      i <- arbitrary
      b <- arbitrary
      node <- elements [ValInt i, ValBool b, Nil]
      return $ node
  arbitrarySizedAst m | otherwise = 
    do 
      l <- arbitrarySizedAst (m `div` 2)
      r <- arbitrarySizedAst (m `div` 2)
      str <- elements ["x","y","z"]
      ifast <- arbitrarySizedIf m
      node <- elements [And l r, Or l r, Not l,
                        Plus l r, Minus l r, Mult l r, Div l r,
                        Cons l r,
                        ifast,
                        Let str l r,
                        Lam str l,
                        App l r,
                        Var str
                        ]
      return node
  -- it would be better if every branch were built like this so the balance would be maintained
  arbitrarySizedIf ::  Int -> Gen Ast
  arbitrarySizedIf m = do b <- arbitrarySizedAst (m `div` 3)
                          t <- arbitrarySizedAst (m `div` 3)
                          e <- arbitrarySizedAst (m `div` 3)
                          return $ If b t e
  
  -- declear cons as a right associative infix 
  infixr 7 `cons`
  cons = Cons
  
  -- | test for if the result carries the error
  errorTest = testGroup "test for basic error handling" [
    -- testProperty "for all ast (except function, because they can be lazy), the eval result should have the same error as the first error encountered" $
    --   \ast -> 
    --     let 
    --       subExpsRes = map (\exp -> runEnvUnsafe (eval exp) Map.empty) $ subexpression ast 
    --       subExpErrors = filter isError subExpsRes
    --       finalRes = runEnvUnsafe (eval ast) Map.empty 
    --     in 
    --       not (null subExpErrors) ==>
    --         case finalRes of 
    --           Ok (Fun _) -> True  -- do not test function
    --           _ ->  sameError (head subExpErrors) finalRes,
    
    testCase "type mismatch example" $
      do 
        assertBool "1 + True" $ isError (runEmptyT $ ValInt 1 `Plus` ValBool True)
        assertBool "[] - 2" $ isError (runEmptyT $ Nil `Minus` ValInt 2)
        assertBool "1 and True" $ isError (runEmptyT $ ValInt 1 `And` ValBool True)
        assertBool "[] or False" $ isError (runEmptyT $ Nil `Or` ValBool False)
        assertBool "not []" $ isError (runEmptyT $ Not Nil)
        assertBool "Cons [] 3" $ isError (runEmptyT $ Cons Nil (ValInt 3))
        assertBool "if [] then 2 else 3" $ isError (runEmptyT $ If Nil (ValInt 2) (ValInt 3))
        assertBool "App 1 2" $ isError (runEmptyT $ (ValInt 1) `App` (ValInt 2))
    ]

  evalTest = testGroup "test for evaluation function" [
    testCase "evaluation should support multityped list" $ 
      do 
        assertEqual "[1, 2, True]" (Ok $ TLs [TI 1, TI 2, TB True]) 
          (runEmptyT $ ValInt 1 `cons` ValInt 2 `cons` ValBool True `cons` Nil)
        assertEqual "[1, True, [False, 2]]" (Ok $ TLs [TI 1, TB True, TLs [TB False, TI 2]]) 
          (runEmptyT $ ValInt 1 `cons` ValBool True `cons` (ValBool False `cons` ValInt 2 `cons` Nil) `Cons` Nil),

    testCase "division by 0" $
      assertBool "1/0" $ isError (runEmptyT $ ValInt 1 `Div` ValInt 0),

    
    testProperty "function in list example: [True, 1, \\x -> x]" $
      \inp -> 
        let 
          res = runEmpty $ ValBool True `cons` ValInt 1 `cons` Lam "x" (Var "x") `cons` Nil
        in 
          case res of 
            Ok (Ls [B True, I 1, Fun f]) -> (f $ I inp) `eqInt` (Ok $ I $ inp)
            _ -> False,
    
    testProperty "nested function: \\x -> \\y -> x + y" $
      \x y ->
        let 
          res =  runEmpty $ Lam "x" $ Lam "y" $ (Var "x") `Plus` (Var "y")
        in 
          (res `appFunc` Ok (I x) `appFunc` Ok (I y)) `eqInt` (Ok $ I $ x + y),
    
    testProperty "function as input: \\f -> \\x -> f x" $ 
      \(QC.Fun _ f :: QC.Fun Integer Integer) n -> 
        let 
          res =  runEmpty $ Lam "f"  $ Lam "x" $ (Var "f") `App` (Var "x")
          fVal inp = 
            case inp of 
              I n -> Ok $ I $ f n 
              _ -> Error "f only handles Integer"
        in 
          (res `appFunc` Ok (Fun fVal) `appFunc` Ok (I n)) `eqInt` (Ok $ I $ f n)
    ]

  stdLibTest = testGroup "test for standard library" [
    testCase "test for head function" $ 
      do 
        assertBool "for a empty list, the head should return an error" $ isError $ run (Var "head" `App` Nil)
        assertBool "head of [1, \\x -> x] should be 1" $ run (Var "head" `App` (ValInt 1 `cons` Lam "x" (Var "x") `cons` Nil)) `eqInt` (Ok $ I 1),
    
    testCase "test for len function" $ 
      do 
        assertBool "for a empty list, the length shold be 0" $ run (Var "len" `App` Nil) `eqInt` (Ok $ I $ 0)
        assertBool "length of [True, 1, \\x -> x, False] should be 4" $  
          run (Var "len" `App` (ValBool True `cons` ValInt 1 `cons` Lam "x" (Var "x") `cons` ValBool False `cons` Nil)) `eqInt` (Ok $ I 4)
  
    ]
