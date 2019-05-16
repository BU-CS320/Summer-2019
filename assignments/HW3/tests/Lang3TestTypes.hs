{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Lang3TestTypes (Ast(..), eval, VarName, varNameToString) where 

    import Lang3 (Ast(..), eval)
    import Test.Tasty.QuickCheck

    data VarName = VarName String

    arbitraryVarName :: Gen VarName
    arbitraryVarName = do str <- elements ["x","y","z"]
                          return $ VarName str

    instance Arbitrary VarName where
        arbitrary = arbitraryVarName
    
    instance Show VarName where
        show (VarName s) = show s

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
                                          node <- elements [LiteralInt i, Var str, Plus l r, Separator l r, Assign str l, Sub l r, Mult l r]
                                          return node

    deriving instance Eq Ast 

    instance Arbitrary Ast where
        arbitrary = sized arbitrarySizedAst2
        shrink (LiteralInt i) = [LiteralInt i' | i' <- shrink i]
        shrink (Var i) = []
        shrink (Plus x y) = [x,y] ++ [Plus x' y' | (x', y') <- shrink (x, y)]
        shrink (Sub x y) = [x,y] ++ [Sub x' y' | (x', y') <- shrink (x, y)]
        shrink (Separator x y) = [x,y] ++ [Separator x' y' | (x', y') <- shrink (x, y)]
        shrink (Mult x y) = [x,y] ++ [Mult x' y' | (x', y') <- shrink (x, y)]
        shrink (Assign varName exp) = [exp] ++ [exp' | exp' <- shrink exp] ++ [Assign varName exp' | exp' <- shrink exp]
