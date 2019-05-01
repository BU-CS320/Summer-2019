{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module MonadTestType (
    BB.List(..), BB.Pair(..), BB.Maybe(..), BB.Either(..), BB.Identity(..), BB.Trival(..),
    PrinterMonad(..), ShowablePrinter(..)
) where

    import Test.Tasty (testGroup, TestTree)
    import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
    import Test.Tasty.QuickCheck

    import GHC.Generics (Generic)
    import Data.Typeable (Typeable)
    import Control.Monad

    import BareBonesLast as BB (List(..), Maybe(Nothing, Just), Either(Left, Right), Identity(Identity), Trival(NoA), Pair(Pair))
    import PrinterMonad

    --translate from List to Prelude
    l2p :: List a -> [a]
    l2p Nil = []
    l2p (Cons x xs) = x:(l2p xs)

    --translate from Prelude to List
    p2l :: [a] -> List a
    p2l [] = Nil
    p2l (x:xs) = Cons x (p2l xs)

    -- translate Maybe to Prelude
    m2p :: BB.Maybe a -> Prelude.Maybe a
    m2p BB.Nothing = Prelude.Nothing
    m2p (BB.Just n) = Prelude.Just n

    -- translate Prelude to Maybe
    p2m :: Prelude.Maybe a -> BB.Maybe a
    p2m Prelude.Nothing = BB.Nothing
    p2m (Prelude.Just n) = BB.Just n

    r2p :: BB.Pair a b -> (a,b)
    r2p (BB.Pair a b) = (a,b)

    p2r :: (a,b) -> BB.Pair a b
    p2r (a,b) = BB.Pair a b
    
    e2p :: BB.Either a b -> Prelude.Either a b
    e2p (BB.Left a) = Prelude.Left a
    e2p (BB.Right b) = Prelude.Right b

    p2e :: Prelude.Either a b -> BB.Either a b
    p2e (Prelude.Left a) = BB.Left a
    p2e (Prelude.Right b) = BB.Right b



    instance (Arbitrary a) => Arbitrary (BB.List a) where
        arbitrary = do ls <- arbitrary; pure $ p2l $ ls
        shrink a =  fmap p2l (shrink $ l2p a)

    instance (Arbitrary a) => Arbitrary (BB.Maybe a) where
        arbitrary = do ls <- arbitrary; pure $ p2m $ ls
        shrink a =  fmap p2m (shrink $ m2p a) 

    instance (Arbitrary a, Arbitrary b) => Arbitrary (BB.Pair a b) where
        arbitrary = do ls <- arbitrary; pure $ p2r $ ls
        shrink a =  fmap p2r (shrink $ r2p a)
        
    instance (Arbitrary a, Arbitrary b) => Arbitrary (BB.Either a b) where
        arbitrary = do ls <- arbitrary; pure $ p2e $ ls
        shrink a =  fmap p2e (shrink $ e2p a) 

    deriving instance Generic (BB.Trival a)
    instance (Arbitrary a) => Arbitrary (BB.Trival a) where
        arbitrary = pure NoA
        shrink = genericShrink 

    deriving instance Generic (BB.Identity a)
    instance (Arbitrary a) => Arbitrary (BB.Identity a) where
        arbitrary = do x <- arbitrary; pure $ BB.Identity x
        shrink = genericShrink 

    data ShowablePrinter out res = ShowablePrinter [out] res deriving Show 
    
    showableToPrinter :: ShowablePrinter out res -> PrinterMonad out res 
    showableToPrinter (ShowablePrinter plist res) = PrinterMonad plist res

    printerToShowable :: PrinterMonad out res -> ShowablePrinter out res 
    printerToShowable (PrinterMonad plist res) = ShowablePrinter plist res

    mapFuncRes :: (a -> b) -> (c -> a) -> (c -> b)
    mapFuncRes f g input = f $ g input

    deriving instance (Eq out, Eq res) => Eq (PrinterMonad out res)
    deriving instance (Eq out, Eq res) => Eq (ShowablePrinter out res)

    instance Functor (ShowablePrinter out) where 
        fmap f a = printerToShowable $ fmap f (showableToPrinter a)

    instance Applicative (ShowablePrinter out) where 
        pure = return
        (<*>) = ap

    instance Monad (ShowablePrinter out) where 
        a >>= f = printerToShowable $ (showableToPrinter a) >>= (mapFuncRes showableToPrinter f)
        return a = printerToShowable $ return a

    instance (Arbitrary out, Arbitrary res) => Arbitrary (ShowablePrinter out res) where
        arbitrary = do pList <- arbitrary; res <- arbitrary; pure $ ShowablePrinter pList res
        shrink (ShowablePrinter pList res) = 
            [ShowablePrinter pList smallerRes | smallerRes <- shrink res] ++
            [ShowablePrinter smallerPList res | smallerPList <- shrink pList] ++
            [ShowablePrinter smallerPList smallerRes | smallerPList <- shrink pList, smallerRes <- shrink res]
    
