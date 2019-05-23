module LangTest where

  import TestBase 

  import Data.Char
  import qualified Lang0 as L0 (Ast(LiteralInt, Plus), eval)
  import qualified Lang1 as L1 (Ast(LiteralInt, Plus, Div), eval)
  import qualified Lang2 as L2 (Ast(LiteralInt,Plus,Print,Separator), eval)
  import qualified Lang3 as L3 (Ast(LiteralInt,Plus,Id,Assign,Separator), eval)
  import qualified Lang4 as L4 (Ast(LiteralInt,Plus,Id,Let), eval)
  import Test.Tasty (testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
  import Test.Tasty.QuickCheck as QC
  import qualified Data.Map as Map

  ---- All Lang Test ----
  langTest = testGroup "Test For all the language Problem" [
      lang0Test,
      lang1Test,
      lang2Test,
      lang3Test,
      lang4Test 
    ]

  lang0Test = testGroup "Test For Lang0" [
     evalWorks0,
     plusWorks0
    ]

  lang1Test = testGroup "Test For Lang1" [
     evalWorks1,
     plusWorks1,
     divZeroWorks1,
     divNumWorks1,
     plus2Works1
     ]

  lang2Test = testGroup "Test For Lang2" [
     evalWorks2,
     plusWorks2,
     plusMoreWorks2,
     seperatorWorks2,
     seperatorMoreWorks2,
     printWorks2,
     printMoreWorks2   
     ]

  lang3Test = testGroup "Test For Lang3" [
     evalWorks3,
     plusWorks3,
     seperatorWorks3,
     assignWorks3,
     assign2Works3,
     assign3Works3,
     assign4Works3,
     idWorksEmpty3,
     idWorks3,
     id2Works3
     ]

  lang4Test = testGroup "Test For Lang4" [
     evalWorks4,
     plusWorks4,
     letWorks4,
     letMoreWorks4,
     letMore2Works4,
     letMore3Works4,
     letMore4Works4,
     let5Works4,
     let6Works4,
     let7Works4,
     idWorks4,
     id2Works4,
     id3Works4     
     ]

  removeSpaces :: String -> String
  removeSpaces [] = []
  removeSpaces (h:t) | isSpace h  = removeSpaces t
                     | otherwise  = h : removeSpaces t
  -- equality on eval result
  instance Eq L0.Ast where
     a == b = (L0.eval a)==(L0.eval b)
  
  lefthp tp = case tp of (a,b) -> a
  -- Lang0
  evalWorks0 = testCase "eval should be the same" $
                assertEqual [] 3 $ L0.eval $ (L0.LiteralInt 3)
  plusWorks0 = testCase "plus should be equal to +" $
                assertEqual [] 3 $ L0.eval $ (L0.LiteralInt 1) `L0.Plus` (L0.LiteralInt 2)

  -- Lang1
  evalWorks1 = testCase "eval should be the same" $
                assertEqual [] (Just 3) $ L1.eval $ (L1.LiteralInt 3)
  plusWorks1 = testCase "plus should be equal to +" $
                assertEqual [] (Just 3) $ L1.eval $ (L1.LiteralInt 1) `L1.Plus` (L1.LiteralInt 2)
  divZeroWorks1 = testCase "div 0 should be equal to Nothing" $
                assertEqual [] Nothing $ L1.eval $ (L1.LiteralInt 1) `L1.Div` (L1.LiteralInt 0)
  divNumWorks1 = testCase "div num should be equal to /" $
                assertEqual [] (Just 2) $ L1.eval $ (L1.LiteralInt 4) `L1.Div` (L1.LiteralInt 2)
  plus2Works1 = testCase "plus 1+(6/2)=4" $
                assertEqual [] (Just 4) $ L1.eval $ (L1.LiteralInt 1) `L1.Plus` ((L1.LiteralInt 6) `L1.Div`(L1.LiteralInt 2))

  
  -- Lang2
  -- eval print(1); print(2)   ==> ([1,2], 2)
  -- eval ((1 + print (10)) + 2 ; print(3)) ==> ([10,3], 3)
  -- eval print(print(print(1) + 2) +3 ) +4  ==> ([1,3,6], 10)
  -- eval print(print(1) + print(2)) ; 7   ==> ([1,2,3], 7)
  evalWorks2 = testCase "eval should be the same" $
                assertEqual [] ([], 3) $ L2.eval $ (L2.LiteralInt 3)
  plusWorks2 = testCase "plus should be equal to +" $
                assertEqual [] ([], 3) $ L2.eval $ (L2.LiteralInt 1) `L2.Plus` (L2.LiteralInt 2)
  plusMoreWorks2 = testCase "plus more should be equal to +" $
                assertEqual [] ([1,2], 3) $ L2.eval $ (L2.Print $ L2.LiteralInt 1) `L2.Plus` (L2.Print $ L2.LiteralInt 2)
  seperatorWorks2 = testCase "Separator print(1); print(2)   ==> ([1,2], 2)" $
                assertEqual [] ([1,2], 2) $ L2.eval $ (L2.Print $ L2.LiteralInt 1) `L2.Separator` (L2.Print $ L2.LiteralInt 2)
  seperatorMoreWorks2 = testCase "Separator ((1 + print (10)) + 2 ; print(3)) ==> ([10,3], 3)" $
                assertEqual [] ([10,3], 3) $ L2.eval $ ((L2.LiteralInt 1 `L2.Plus` (L2.Print $ L2.LiteralInt 10)) `L2.Plus` (L2.LiteralInt 2)) `L2.Separator` (L2.Print $ L2.LiteralInt 3)
  printWorks2 = testCase "Print should be print(print(print(1) + 2) +3 ) +4  ==> ([1,3,6], 10)" $
                assertEqual [] ([1,3,6], 10) $ L2.eval $ (L2.Print $ (L2.Print $ (L2.Print $ L2.LiteralInt 1) `L2.Plus` (L2.LiteralInt 2)) `L2.Plus` (L2.LiteralInt 3)) `L2.Plus` (L2.LiteralInt 4)
  printMoreWorks2 = testCase "Print should be print(print(1) + print(2)) ; 7   ==> ([1,2,3], 7)" $
                assertEqual [] ([1,2,3], 7) $ L2.eval $ (L2.Print $ (L2.Print $ L2.LiteralInt 1) `L2.Plus` (L2.Print $ L2.LiteralInt 2)) `L2.Separator` (L2.LiteralInt 7)
  
  -- Lang3
  evalWorks3 = testCase "eval should be the same" $
                assertEqual [] (Just 5,Map.empty) $ L3.eval (L3.LiteralInt 5) Map.empty
  
  plusWorks3 = testCase "plus should be equal to +" $
              assertEqual [] (Just 3) $ lefthp $ L3.eval (L3.Plus (L3.LiteralInt 1) (L3.LiteralInt 2)) Map.empty

  seperatorWorks3 = testCase "Separator 1;(2;1+2) should be 3" $
              assertEqual [] (Just 3) $ lefthp $ L3.eval ((L3.LiteralInt 1) `L3.Separator` (L3.LiteralInt 2 `L3.Separator` (L3.Plus (L3.LiteralInt 1) (L3.LiteralInt 2)))) Map.empty

  assignWorks3 = testCase "Assign x = (1;(2;1+2))+a ,a=2 shoule be 5" $
              assertEqual [] (Just 5) $ lefthp $ L3.eval (L3.Assign "x" (((L3.LiteralInt 1) `L3.Separator` (L3.LiteralInt 2 `L3.Separator` (L3.Plus (L3.LiteralInt 1) (L3.LiteralInt 2)))) `L3.Plus` (L3.Id "a"))) (Map.fromList [("a",2)])

  ast1 = (L3.Assign "x" (((L3.LiteralInt 1) `L3.Separator` (L3.LiteralInt 2 `L3.Separator` (L3.Plus (L3.LiteralInt 1) (L3.LiteralInt 2)))) `L3.Plus` (L3.Id "a")))
  assign2Works3 = testCase "1+(Assign x = (1;(2;1+2))+a) ,a=2 shoule be 6" $
                 assertEqual [] (Just 6) $ lefthp $ L3.eval ((L3.LiteralInt 1) `L3.Plus` ast1) (Map.fromList [("a",2)])
  
  ast2 = L3.Assign "x" (L3.LiteralInt 2)
  ast3 = L3.Assign "y" (L3.LiteralInt 2)
  ast4 = L3.Assign "x" (L3.LiteralInt 3)
  assign3Works3 = testCase "Assign: x=2; y=2; x=3, x should be 3" $
                  assertEqual [] (Just 3) $ lefthp $ L3.eval ((ast2 `L3.Separator` ast3) `L3.Separator` ast4) Map.empty

  ast5 = (L3.LiteralInt 1) `L3.Plus` ast2                
  assign4Works3 = testCase "x = 1 + (x = 2), x should be 3" $
                  assertEqual [] (Just 3) $ lefthp $ L3.eval (L3.Assign "x" ast5) Map.empty
  
  idWorksEmpty3 = testCase "Id Empty should be Nothing" $
            assertEqual [] (Nothing, Map.empty) $ L3.eval (L3.Id "") $ Map.empty

  idWorks3 = testCase "Id should be match the map" $
            assertEqual [] (Just 2) $ lefthp $ L3.eval (L3.Id "a") (Map.fromList [("a",2)])
            
  id2Works3 = testCase "Id not in scope should be nothing" $
            assertEqual [] Nothing $ lefthp $ L3.eval (L3.Id "b") (Map.fromList [("x",2),("y",1)]) 
 

  -- Lang4
  evalWorks4 = testCase "eval should be the same" $
              assertEqual [] (Just 5) $ L4.eval (L4.LiteralInt 5) Map.empty
  
  plusWorks4 = testCase "plus should be equal to +" $
              assertEqual [] (Just 3) $ L4.eval ((L4.Id "x") `L4.Plus` (L4.Id "y")) (Map.fromList [("x",2),("y",1)])

  letWorks4 = testCase "Let should be 'let var = 1+2 in var'" $
              assertEqual [] (Just 3) $ L4.eval (L4.Let "var" ((L4.LiteralInt 1) `L4.Plus` (L4.LiteralInt 2)) (L4.Id "var")) Map.empty
  
  letMoreWorks4 = testCase "Let should be 'let var = x+2 in (var+1)'" $
              assertEqual [] Nothing $ L4.eval (L4.Let "var" ((L4.Id "x") `L4.Plus` (L4.LiteralInt 2)) (L4.Id "var" `L4.Plus` (L4.LiteralInt 1))) Map.empty
  
  letMore2Works4 = testCase "Let should be 'let var = x+2 in (var+1)'" $
              assertEqual [] (Just 5) $ L4.eval (L4.Let "var" ((L4.Id "x") `L4.Plus` (L4.LiteralInt 2)) (L4.Id "var" `L4.Plus` (L4.LiteralInt 1))) (Map.fromList [("x",2),("y",1)])
              
  lets = (L4.Let "var" ((L4.Id "x") `L4.Plus` (L4.LiteralInt 2)) (L4.Id "var" `L4.Plus` (L4.LiteralInt 1)))
  letMore3Works4 = testCase "Let should be 'let var = (let var = x+2 in var+2) in (var+1)'" $
              assertEqual [] (Just 6) $ L4.eval (L4.Let "var" lets (L4.Id "var" `L4.Plus` (L4.LiteralInt 1))) (Map.fromList [("x",2),("y",1)])
  
  letMore4Works4 = testCase "Let should be 'x+(let var = x+2 in var+2)=,x=2'" $
              assertEqual [] (Just 7) $ L4.eval ((L4.Id "x") `L4.Plus` lets) (Map.fromList [("x",2),("y",1)]) 
  
  ast41 = L4.Let "x" (L4.LiteralInt 2) (L4.Id "x") 
  let5Works4 = testCase "Let out of scope: x + let x=2 in x, should be nothing" $
              assertEqual [] Nothing $ L4.eval ((L4.Id "x") `L4.Plus` ast41) (Map.empty)

  ast42 = L4.Let "y" (L4.LiteralInt 1) (L4.Id "x" `L4.Plus` L4.Id "y") 
  ast43 = L4.Let "x" (L4.LiteralInt 2) ast42
  let6Works4 = testCase "nested let: let x=2 in (let y=1 in x+y), should be 3" $
                 assertEqual [] (Just 3) $ L4.eval ast43 (Map.empty)

  ast44 = L4.Let "x" (L4.LiteralInt 3) (L4.Id "x")
  ast45 = L4.Let "x" (L4.LiteralInt 2) ast44
  let7Works4 = testCase "let reassign: let x=2 in (let x = 3 in x), should be 3" $
               assertEqual [] (Just 3) $ L4.eval ast45 (Map.empty)

  idWorks4 = testCase "Id should show its value" $
              assertEqual [] Nothing $ L4.eval (L4.Id "x") (Map.empty)            

  id2Works4 = testCase "Id should show its value" $
              assertEqual [] (Just 2) $ L4.eval (L4.Id "x") (Map.fromList [("x",2),("y",1)]) 
  
  id3Works4 = testCase "Id not in scope should be nothing" $
              assertEqual [] Nothing $ L4.eval (L4.Id "b") (Map.fromList [("x",2),("y",1)]) 
   