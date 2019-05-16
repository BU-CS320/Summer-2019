module Lang1Test where

    import Lang1 (Ast(..), eval)
    import Lang1TestTypes
    import Test.Tasty (testGroup, TestTree)
    import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
    import Test.Tasty.QuickCheck

    
    safeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
    safeAdd (Just x) (Just y) = Just (x + y)
    safeAdd _ _ = Nothing

    safeSub :: Maybe Integer -> Maybe Integer -> Maybe Integer
    safeSub (Just x) (Just y) = Just (x - y)
    safeSub _ _ = Nothing
    
    safeDiv :: Maybe Integer -> Maybe Integer -> Maybe Integer
    safeDiv (Just x) (Just 0) = Nothing
    safeDiv (Just x) (Just y) = Just (x `div` y)
    safeDiv _ _ = Nothing
    
    safeMult :: Maybe Integer -> Maybe Integer -> Maybe Integer
    safeMult (Just x) (Just y) = Just (x * y)
    safeMult _ _ = Nothing

    subExpression :: Ast -> [Ast]
    subExpression (exp1 `Plus` exp2) = [exp1, exp2]
    subExpression (exp1 `Div` exp2) = [exp1, exp2]
    subExpression (exp1 `Sub` exp2) = [exp1, exp2]
    subExpression (exp1 `Mult` exp2) = [exp1, exp2]
    subExpression (LiteralInt n) = []


    lang1Test = testGroup "Lang1 Test" [
        baseCase,
        plusTest,
        subTest,
        divTest,
        multTest,
        nothingTest
        ]    
    
    baseCase = testProperty "Base case: for all integer i, evaluation of LiteralInt i should be Just i." $
                \i -> eval (LiteralInt (i::Integer)) == (Just i)
    
    plusTest = testProperty "For all ast1 & ast2, `Plus ast1 ast2` should evaluate to `(eval ast1) + (eval ast2)`" $
                \ast1 ast2 -> eval (Plus ast1 ast2) == safeAdd (eval ast1) (eval ast2)
                
    subTest = testProperty "For all ast1 & ast2, `Sub ast1 ast2` should evaluate to `(eval ast1) - (eval ast2)`" $
                \ast1 ast2 -> eval (Sub ast1 ast2) == safeSub (eval ast1) (eval ast2)
                
    divTest = testProperty "For all ast1 & ast2, `Div ast1 ast2` should evaluate to `(eval ast1) / (eval ast2)` where divide by zero returns Nothing" $
                \ast1 ast2 -> eval (Div ast1 ast2) == safeDiv (eval ast1) (eval ast2)
                
    multTest = testProperty "For all ast1 & ast2, `Mult ast1 ast2` should evaluate to `(eval ast1) * (eval ast2)`" $
                \ast1 ast2 -> eval (Mult ast1 ast2) == safeMult (eval ast1) (eval ast2)

    nothingTest = testProperty "For all ast, if any of the sub expression is Nothing, the ast should evaluate to nothing" $
                    \ast -> (any (== Nothing) . map eval $ (subExpression ast)) ==> (eval ast == Nothing)