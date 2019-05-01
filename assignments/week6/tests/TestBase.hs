{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module TestBase (
    L0Ast, L1Ast, L2Ast, L3Ast, L4Ast
    ) where

    import GHC.Generics (Generic)
    import Data.Typeable (Typeable)
    import Test.QuickCheck.Arbitrary.Generic

    import qualified Lang0 as L0 (Ast(LiteralInt, Plus), eval)
    import qualified Lang1 as L1 (Ast(LiteralInt, Plus, Div), eval)
    import qualified Lang2 as L2 (Ast(LiteralInt, Plus, Print, Separator), eval)
    import qualified Lang3 as L3 (Ast(LiteralInt, Plus, Var, Assign, Separator), eval)
    import qualified Lang4 as L4 (Ast(LiteralInt, Plus, Var, Let), eval)
    
    import Lang0Parser as L0Parser (parser)
    import Lang1Parser as L1Parser (parser)
    import Lang2Parser as L2Parser (parser)
    import Lang3Parser as L3Parser (parser)
    import Lang4Parser as L4Parser (parser)

    type L0Ast = L0.Ast
    type L1Ast = L1.Ast
    type L2Ast = L2.Ast
    type L3Ast = L3.Ast
    type L4Ast = L4.Ast

    deriving instance Generic L0Ast 
    deriving instance Eq L0Ast 

    deriving instance Generic L1Ast 
    deriving instance Eq L1Ast 

    deriving instance Generic L2Ast 
    deriving instance Eq L2Ast 

    deriving instance Generic L3Ast 
    deriving instance Eq L3Ast 

    deriving instance Generic L4Ast 
    deriving instance Eq L4Ast 

    instance Arbitrary L0Ast where
        arbitrary = genericArbitrary
        shrink = genericShrink

    instance Arbitrary L1Ast where
        arbitrary = genericArbitrary
        shrink = genericShrink
    
    instance Arbitrary L2Ast where
        arbitrary = genericArbitrary
        shrink = genericShrink

    instance Arbitrary L3Ast where
        arbitrary = genericArbitrary
        shrink = genericShrink

    instance Arbitrary L4Ast where
        arbitrary = genericArbitrary
        shrink = genericShrink
