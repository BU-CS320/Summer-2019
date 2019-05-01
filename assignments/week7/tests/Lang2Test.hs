module Lang2Test where

    import Lang2TestTypes
    import Test.Tasty (testGroup, TestTree)
    import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
    import Test.Tasty.QuickCheck

    import PrinterMonad (PrinterMonad(..), runPrinterMonad)


    getPrinting :: PrinterMonad Integer Integer -> [Integer]
    getPrinting (PrinterMonad printList result) = printList

    evalAndGetPrinting :: Ast -> [Integer]
    evalAndGetPrinting = getPrinting . eval

    getResult :: PrinterMonad Integer Integer -> Integer
    getResult (PrinterMonad printList result) = result

    evalAndGetResult :: Ast -> Integer
    evalAndGetResult = getResult . eval


    printTest = testProperty "For all integer i, eval LiteralInt i should not print anything" $
                \ i -> ((evalAndGetPrinting $ LiteralInt i) :: [Integer]) == []

    operatorTest = testGroup "Prove printing correct" [
        testProperty "For two expression exp1 exp2, the print list for `Plus exp1 exp2` should be the print list of exp1 ++ print list of exp2" $
        \ i j -> ((evalAndGetPrinting $ Plus i j) :: [Integer]) == (evalAndGetPrinting i) ++ (evalAndGetPrinting j),

        testProperty "For two expression exp1 exp2, the print list for `Sub exp1 exp2` should be the print list of exp1 ++ print list of exp2" $
        \ i j -> ((evalAndGetPrinting $ Sub i j) :: [Integer]) == (evalAndGetPrinting i) ++ (evalAndGetPrinting j),

        testProperty "For two expression exp1 exp2, the print list for `Mult exp1 exp2` should be the print list of exp1 ++ print list of exp2" $
        \ i j -> ((evalAndGetPrinting $ Mult i j) :: [Integer]) == (evalAndGetPrinting i) ++ (evalAndGetPrinting j),

        testProperty "For two expression exp1 exp2, the print list for `Separator exp1 exp2` should be the print list of exp1 ++ print list of exp2" $
        \ i j -> ((evalAndGetPrinting $ Separator i j) :: [Integer]) == (evalAndGetPrinting i) ++ (evalAndGetPrinting j),

        testProperty "For expression exp, the print list for `Print exp` should be the print list of exp ++ [value of eval exp]" $
        \ exp -> ((evalAndGetPrinting $ Print exp) :: [Integer]) == (evalAndGetPrinting exp ++ [evalAndGetResult exp])
        ]

    resultTest = testGroup "Prove result correct" [
        testProperty "For all integer i, evaluate LiteralInt i should be evaluate to i" $
        \i -> i == (evalAndGetResult $ LiteralInt i),

        testProperty "For all ast1 and ast2, `Plus ast1 ast2` should be evaluate to `(eval ast1) + (eval ast2)`" $
        \ i j -> (evalAndGetResult $ Plus i j) == (evalAndGetResult i) + (evalAndGetResult j),

        testProperty "For all ast1 and ast2, `Sub ast1 ast2` should be evaluate to `(eval ast1) - (eval ast2)`" $
        \ i j -> (evalAndGetResult $ Sub i j) == (evalAndGetResult i) - (evalAndGetResult j),

        testProperty "For all ast1 and ast2, `Mult ast1 ast2` should be evaluate to `(eval ast1) * (eval ast2)`" $
        \ i j -> (evalAndGetResult $ Mult i j) == (evalAndGetResult i) * (evalAndGetResult j),

        testProperty "For all ast1 and ast2, `Separator ast1 ast2` should be evaluate to `(eval ast2)`" $
        \ i j -> (evalAndGetResult $ Separator i j) == (evalAndGetResult j),

        testProperty "For all ast, `Print ast` should be evaluate to `(eval ast)`" $
        \ ast -> (evalAndGetResult $ Print ast) == (evalAndGetResult ast)
        ]

    lang2Test = testGroup "Lang2 Test" [
        printTest,
        operatorTest,
        resultTest
        ]