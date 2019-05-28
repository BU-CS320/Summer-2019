{-# LANGUAGE StandaloneDeriving #-}
module BareBonesAgainTest where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
import Test.Tasty.QuickCheck

import BareBonesAgain (List(..), isEmpty, (++), reverse, concat, take, map, multiplyEachBy7, filter,keepEvens, 
  Pair(..), fst, snd, zip, head, last, (!!),
  Maybe(..))
import Data.List -- TODO: more standard import from prelude?

deriving instance Eq a => Eq (BareBonesAgain.List a)
deriving instance (Eq a, Eq b) => Eq (BareBonesAgain.Pair a b)
deriving instance Eq a => Eq (BareBonesAgain.Maybe a)

-- TODO: this could all be cleaned up with better names given

tests = testGroup "BareBonesAgainTest" [
 testCase "isEmpty Nil" $ assertBool "" (isEmpty Nil),
 testCase "isEmpty (Cons 1 Nil)" $ assertBool "" (not $ isEmpty (Cons 1 Nil)),
 
 testProperty "For all lists, append should equal Prelude's append" $
   \la lb -> (la::[Integer]) Data.List.++ (lb::[Integer]) == (l2p ((p2l la) BareBonesAgain.++ (p2l lb))),
 testProperty "For all lists, reverse should equal Prelude.reverse" $
    \la -> (BareBonesAgain.reverse $ p2l (la::[Integer])) == (p2l $ Data.List.reverse la),
 testProperty "For all lists, concat should equal Prelude.concat" $ 
   \la -> (Prelude.concat la :: [Bool]) == (l2p $ BareBonesAgain.concat $ p2l (Prelude.map p2l la)),
 testProperty "For all lists, take should equal Prelude.take" $
   \n la -> n >= 0 ==> (BareBonesAgain.take n $ p2l la) == (p2l $ (Data.List.take (fromIntegral n) $ la::[Bool])),
 testProperty "For all lists, map should equal Prelude.map" $
   \la -> (BareBonesAgain.map not $ p2l la) == (p2l $ Data.List.map not (la::[Bool])),
  testCase "multiplyEachBy7 (Cons 0 (Cons 1 (Cons (-1) Nil)))" $ assertEqual  ""  (Cons 0 (Cons 7 (Cons (-7) Nil))) $ 
     multiplyEachBy7 (Cons 0 (Cons 1 (Cons (-1) Nil))),
 testProperty "For all lists, filter should equal Prelude.map" $
   \la -> (BareBonesAgain.filter (>2) $ p2l la) == (p2l $ Data.List.filter (>2) (la::[Integer])),
  testCase "keepEvens (Cons 0 (Cons 1 (Cons (-10) Nil)))" $ assertEqual  ""  (Cons 0 (Cons (-10) Nil)) $
     BareBonesAgain.keepEvens (Cons 0 (Cons 1 (Cons (-10) Nil))),
	 
	 
  testCase "fst (Pair True 'a')" $ assertEqual  ""  True $ 
     BareBonesAgain.fst (Pair True 'a'),
  testCase "snd (Pair True 'a')" $ assertEqual  ""  'a' $ 
     BareBonesAgain.snd (Pair True 'a'),
	 
  testProperty  "zip should work the same as Prelude.zip" $ 
   \ll lr -> BareBonesAgain.zip (p2l ll) (p2l lr) ==  (p2l $ Data.List.map (\ (l,r) -> Pair l r) $ Data.List.zip (ll::[Integer]) (lr::[Bool])),
   
  testCase "head Nil" $ assertEqual  ""  BareBonesAgain.Nothing $ 
     BareBonesAgain.head (Nil :: List Bool),
  testCase "head (Cons 0 (Cons 1 (Cons (-1) Nil)))" $ assertEqual  ""  (BareBonesAgain.Just 0) $ 
     BareBonesAgain.head (Cons 0 (Cons 1 (Cons (-1) Nil))),
	 
  testCase "last Nil" $ assertEqual  ""  BareBonesAgain.Nothing $ 
     BareBonesAgain.last (Nil :: List Bool),
  testCase "last (Cons 0 (Cons 1 (Cons (-1) Nil)))" $ assertEqual  ""  (BareBonesAgain.Just (-1)) $ 
     BareBonesAgain.last (Cons 0 (Cons 1 (Cons (-1) Nil))),
	 
  testCase " (Cons 0 (Cons 1 (Cons (-1) Nil))) !! 1 " $ assertEqual  ""  (BareBonesAgain.Just (1)) $ 
    ((Cons 0 (Cons 1 (Cons (-1) Nil))) BareBonesAgain.!! 1), -- TODO: this could be a property test
  testCase " (Cons 0 (Cons 1 (Cons (-1) Nil))) !! 3 " $ assertEqual  ""  BareBonesAgain.Nothing $ 
    ((Cons 0 (Cons 1 (Cons (-1) Nil))) BareBonesAgain.!! 3)
  ]


--translate from List to Prelude
l2p :: List a -> [a]
l2p Nil = []
l2p (Cons x xs) = x:(l2p xs)

--translate from Prelude to List
p2l :: [a] -> List a
p2l [] = Nil
p2l (x:xs) = Cons x (p2l xs)

