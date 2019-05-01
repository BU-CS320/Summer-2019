{-# LANGUAGE ScopedTypeVariables #-}
module ReaderTest where

  import Test.Tasty (defaultMain, testGroup, TestTree)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
  import Test.Tasty.QuickCheck as QC

  import Reader
  import ReaderTestType

  readerTest = testGroup "Reader test" [
    leftMonadLaw,
    rightMonadLaw,
    associativityMonadLaw
    ]

  mapResult::(a -> b) -> (c -> a) -> (c -> b)
  mapResult f g input = f $ g input


  leftMonadLaw = testProperty "left identity law for Reader Monads: forall res:: Integer, init:: Bool, f :: Integer -> Reader Bool Integer. (runReader (return res >>= f) init) == runReader (f res) init" $
      \(init:: Bool) (res:: Integer) (Fun _ f:: Fun Integer (ShowableReader Bool Integer)) -> 
        let f' = mapResult showableToReader f
          in 
            (runReader (return res >>=  f') init) == (runReader ( f' res) init)

  rightMonadLaw = testProperty "left identity law for Reader Monads: forall res:: Reader Bool Integer, init:: Bool. (runReader (res >>= return) init) == (runReader res init) " $
      \(init:: Bool) (res:: ShowableReader Bool Integer) ->
        let reader = showableToReader res
         in 
          (runReader (reader >>= return) init) == (runReader (reader) init)
        
  associativityMonadLaw = testProperty "associativity law for Reader Monads: forall res:: Reader Bool Integer, init:: Bool, f:: Integer -> Reader Bool Integer, f1:: Integer -> Reader Bool Integer. (runReader (res >>= f >>= f1) init) == (runReader (res >>= (\\x -> f x >>= f1)) init)" $ 
      \(init:: Bool) (res:: ShowableReader Bool Integer) (Fun _ f:: Fun Integer (ShowableReader Bool Integer)) (Fun _ f1:: Fun Integer (ShowableReader Bool Integer)) ->
        let f' = mapResult showableToReader f
            f1' = mapResult showableToReader f1
            reader = showableToReader res
          in
            (runReader (reader >>= f' >>= f1') init) == (runReader (reader >>= (\x -> f' x >>= f1')) init)