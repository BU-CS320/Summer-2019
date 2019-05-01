{-# LANGUAGE ScopedTypeVariables #-}
module StateTest where

  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck as QC
  
  import State
  import StateTestType 

  stateTest = testGroup "State test" [
    leftMonadLaw,
    rightMonadLaw,
    associativityMonadLaw
    ]

  mapResult:: (a -> b) -> (c -> a) -> (c -> b)
  mapResult f g input = f $ g input


  leftMonadLaw = testProperty "left identity law for State Monads: forall res:: Integer, init:: Bool, f :: Integer -> State Bool Integer. (runState (return res >>= f) init) == runState (f res) init" $
      \(init:: Bool) (res:: Integer) (Fun _ f:: Fun Integer (ShowableState Bool Integer)) -> 
        let f' = mapResult showableToState f
          in 
            (runState (return res >>=  f') init) == (runState ( f' res) init)
  
  rightMonadLaw = testProperty "left identity law for State Monads: forall res:: State Bool Integer, init:: Bool. (runState (res >>= return) init) == (runState res init) " $
      \(init:: Bool) (res:: ShowableState Bool Integer) ->
        let state = showableToState res
         in 
          (runState (state >>= return) init) == (runState (state) init)
  
  associativityMonadLaw = testProperty "associativity law for State Monads: forall res:: State Bool Integer, init:: Bool, f:: Integer -> State Bool Integer, f1:: Integer -> State Bool Integer. (runState (res >>= f >>= f1) init) == (runState (res >>= (\\x -> f x >>= f1)) init)" $ 
      \(init:: Bool) (res:: ShowableState Bool Integer) (Fun _ f:: Fun Integer (ShowableState Bool Integer)) (Fun _ f1:: Fun Integer (ShowableState Bool Integer)) ->
        let f' = mapResult showableToState f
            f1' = mapResult showableToState f1
            state = showableToState res
          in
            (runState (state >>= f' >>= f1') init) == (runState (state >>= (\x -> f' x >>= f1')) init)
