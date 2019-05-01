{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module TestBase where 

import HigherOrderProblems (Pet(..), FootBallStat(..), isCat, isDog, name)
import Map (Map)
import qualified Map as HwMap
import Data.Map
import Data.Function
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Test.QuickCheck.Arbitrary.Generic

---- Define type class ----
-- define iso type class
class Iso a b where 
  isoMap :: a -> b 


---- Define instance of type classes ----
-- define type class for Pets
instance Show Pet where 
  show p = petType ++ " with name: " ++ name p
    where 
      petType = if isCat p then "cat" 
              else if isDog p then "dog"
              else undefined


instance Eq Pet where  -- Probably don't make sense...
  p == p' = (bothCat || bothDog) && (name p == name p')
    where 
      bothCat = isCat p && isCat p'
      bothDog = isDog p && isDog p'

-- define generic and arbtrary on FootBallStat
deriving instance Generic FootBallStat 
deriving instance Eq FootBallStat 

instance Arbitrary FootBallStat where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- define iso on list to list
instance (Iso a b) => Iso [a] [b] where 
  isoMap = fmap isoMap

instance (Iso a b) => Iso (Maybe a) (Maybe b) where 
  isoMap = fmap isoMap

instance (Iso a1 a2, Iso b1 b2) => Iso (a1, b1) (a2, b2) where
  isoMap (a, b) = (isoMap a, isoMap b)

-- define the map between Map and Data.Map.Map
hwMapToHaskMap :: (Ord k) => HwMap.Map k a -> Data.Map.Map k a
hwMapToHaskMap = Data.Map.fromList . HwMap.toList
haskMapToHwMap :: (Ord k) => Data.Map.Map k a -> HwMap.Map k a
haskMapToHwMap = HwMap.fromList . Data.Map.toList

instance (Iso k1 k2, Iso a1 a2, Ord k1, Ord k2) => Iso (HwMap.Map k1 a1) (Data.Map.Map k2 a2) where 
  isoMap = Data.Map.fromList . isoMap. HwMap.toList

instance (Iso k1 k2, Iso a1 a2, Ord k1, Ord k2) => Iso (Data.Map.Map k1 a1) (HwMap.Map k2 a2) where 
  isoMap = HwMap.fromList . isoMap . Data.Map.toList

instance (Arbitrary a, Ord k, Arbitrary k) => Arbitrary (HwMap.Map k a) where
  arbitrary = do map <- arbitrary; pure $ haskMapToHwMap $ map 

-- define iso on any same type
instance Iso Integer Integer where 
  isoMap = id

instance Iso Int Int where 
  isoMap = id

instance Iso Bool Bool where 
  isoMap = id  

instance Iso Char Char where 
  isoMap = id  

instance Iso Pet Pet where 
  isoMap = id  

instance Iso FootBallStat FootBallStat where 
  isoMap = id  

-- Helper functions

-- this function test a function by existing isomorphic function
-- it feeds the testing function and the existing function with isomorphic parameters
-- and their return should be also isomorphic:
-- idea: if A isomorphic to B, and f: A -> A is isomorphic to g: B -> B
--       then f(a) needs to be isomorphic to g(b)
testOneArgFuncByIso :: 
  (Iso aI bI, Iso aO bO, Eq bO, Show bI, Show bO) => 
  (aI -> aO) ->  -- the function you implemented
  (bI -> bO) ->  -- the existing function that is isomorphic to your function
  aI ->  -- the input for the function you want to test
  Bool
testOneArgFuncByIso f g input =  
  (g . isoMap $ input) == (isoMap . f $ input)

testTwoArgFuncByIso :: 
  (Iso aI1 bI1, Iso aI2 bI2, Iso aO bO, Eq bO, Show bO, Show aO, Show aI1, Show aI2) => 
  (aI1 -> aI2 -> aO) ->  -- the function you implemented
  (bI1 -> bI2 -> bO) ->  -- the existing function that is isomorphic to your function
  aI1 -> aI2 ->  -- the input for the function you want to test
  Bool
testTwoArgFuncByIso f g input1 input2 = 
  (g  (isoMap input1) (isoMap input2)) == (isoMap $ f input1 input2)

