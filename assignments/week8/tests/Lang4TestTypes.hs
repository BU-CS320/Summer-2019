{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Lang4TestTypes (Ast(..), eval,VarName,varNameToString) where 

    import Lang4 (Ast(..), eval)
    import Test.Tasty.QuickCheck
    
    data VarName = VarName String deriving (Show,Eq)

    arbitraryVarName :: Gen VarName
    arbitraryVarName = do str <- elements ["x","y","z"]
                          return $ VarName str

    instance Arbitrary VarName where
        arbitrary = arbitraryVarName

    varNameToString :: VarName -> String
    varNameToString (VarName v) = v

    arbitrarySizedAst2 ::  Int -> Gen Ast
    arbitrarySizedAst2 m | m < 1 = do i <- arbitrary
                                      str <- elements ["x","y","z"]
                                      elements [LiteralInt i, Var str]                   
    arbitrarySizedAst2 m | otherwise = do l <- arbitrarySizedAst2 (m `div` 2)
                                          r <- arbitrarySizedAst2 (m `div` 2)
                                          i <- arbitrary
                                          str <- elements ["x","y","z"]
                                          node <- elements [LiteralInt i, Var str, Plus l r, Let str l r, Sub l r, Mult l r]
                                          return node

    deriving instance Eq Ast 

    instance Arbitrary Ast where
        arbitrary = sized arbitrarySizedAst2
        shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
        shrink (Var i) = []
        shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
        shrink (Sub x y) = [x,y] ++ [Sub x' y' | (x', y') <- shrink (x, y)]
        shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]
        shrink (Let varName varExp exp) = [varExp,exp] ++ [varExp'| varExp' <- shrink varExp] 
                                        ++ [exp'| exp' <- shrink exp] 
                                        ++ [Let varName varExp' exp'| (varExp', exp') <- shrink (varExp, exp)] 
                                        ++ [Let varName varExp exp'| exp' <- shrink exp] 
                                        ++ [Let varName varExp' exp| varExp' <- shrink varExp]
