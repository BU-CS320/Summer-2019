{-# LANGUAGE ScopedTypeVariables #-}
module EnvUnsafeTest where

  import Test.Tasty (testGroup)
  import Test.Tasty.QuickCheck
  import EnvUnsafe (EnvUnsafe(..), Unsafe(..), runEnvUnsafe)

  -- | defines a data structure that can be converted to env unsafe
  -- this is a shrinkable and showable version of env unsafe
  -- very helpful in quick check testing
  data ShowableEnvUnsafe e a = ShowableEnvUnsafe (Fun e (Unsafe a)) deriving Show

  -- | a helper function to cast the showable envunsafe we defined
  -- into the regular env unsafe
  showableToEnvUnsafe :: ShowableEnvUnsafe e a -> EnvUnsafe e a
  showableToEnvUnsafe (ShowableEnvUnsafe (Fun _ f)) = EnvUnsafe f

  -- | changes the function result
  mapResult::(a -> b)  -- ^ indicates how to change result
             -> (c -> a)  -- ^ change the reslt of this function
             -> (c -> b) 
  mapResult f g input = f $ g input

  instance (Arbitrary a) => Arbitrary (Unsafe a) where 
    arbitrary = do aRes <- arbitrary; msg <- arbitrary; elements [Ok aRes, Error msg]
    shrink (Ok aRes) = [Ok aRes'| aRes' <- shrink aRes] 
    shrink (Error msg) = [Error msg' | msg' <- shrink msg]

  instance (CoArbitrary s, Function s, Arbitrary a, Arbitrary s) => Arbitrary (ShowableEnvUnsafe s a) where
    arbitrary = do fun <- arbitrary; return $ ShowableEnvUnsafe fun 
    shrink (ShowableEnvUnsafe fun) = [ShowableEnvUnsafe fun' | fun' <- shrink fun]
  
  -- | a list of tests for the monad laws
  monadLawTest = testGroup "EnvUnsafe test" [
    leftMonadLaw,
    rightMonadLaw,
    associativityMonadLaw
    ]

  leftMonadLaw = testProperty "left identity law for EnvUnsafe Monads: forall res:: Integer, init:: [Bool], f :: Integer -> EnvUnsafe [Bool] Integer. (runEnvUnsafe (return res >>= f) init) == runEnvUnsafe (f res) init" $
    \(init:: [Bool]) (res:: Integer) (Fun _ f:: Fun Integer (ShowableEnvUnsafe [Bool] Integer)) -> 
      let 
        f' = mapResult showableToEnvUnsafe f
      in 
        (runEnvUnsafe (return res >>=  f') init) == (runEnvUnsafe (f' res) init)

  rightMonadLaw = testProperty "left identity law for EnvUnsafe Monads: forall res:: EnvUnsafe [Bool] Integer, init:: [Bool]. (runEnvUnsafe (res >>= return) init) == (runEnvUnsafe res init) " $
    \(init:: [Bool]) (res:: ShowableEnvUnsafe [Bool] Integer) ->
      let 
        envUnsafe = showableToEnvUnsafe res
      in 
        (runEnvUnsafe (envUnsafe >>= return) init) == (runEnvUnsafe (envUnsafe) init)
        
  associativityMonadLaw = testProperty "associativity law for EnvUnsafe Monads: forall res:: EnvUnsafe [Bool] Integer, init:: [Bool], f:: Integer -> EnvUnsafe [Bool] Integer, f1:: Integer -> EnvUnsafe [Bool] Integer. (runEnvUnsafe (res >>= f >>= f1) init) == (runEnvUnsafe (res >>= (\\x -> f x >>= f1)) init)" $ 
    \(init:: [Bool]) (res:: ShowableEnvUnsafe [Bool] Integer) (Fun _ f:: Fun Integer (ShowableEnvUnsafe [Bool] Integer)) (Fun _ f1:: Fun Integer (ShowableEnvUnsafe [Bool] Integer)) ->
      let 
        f' = mapResult showableToEnvUnsafe f
        f1' = mapResult showableToEnvUnsafe f1
        envUnsafe = showableToEnvUnsafe res
      in 
        (runEnvUnsafe (envUnsafe >>= f' >>= f1') init) == (runEnvUnsafe (envUnsafe >>= (\x -> f' x >>= f1')) init)

  -- | a list of tests for the functor laws
  functorLawTest = testGroup "functor law" [
      identityLaw,
      compositionLaw
    ]
  
  identityLaw = testProperty "functor preserve id: for all init:: [Bool], res:: EnvUnsafe [Bool] Integer. runEnvUnsafe (fmap id res) init == runEnvUnsafe res init" $
    \(init:: [Bool]) (res:: ShowableEnvUnsafe [Bool] Integer) -> 
      let
        res' = showableToEnvUnsafe res 
      in
        runEnvUnsafe (fmap id res') init == runEnvUnsafe res' init

  compositionLaw = testProperty "functor preserve composition: init:: [Bool], res:: EnvUnsafe [Bool] Integer, f1 :: Integer -> Bool, f2 :: Bool -> Integer, runEnvUnsafe (fmap (f2 . f1) res) init == runEnvUnsafe (fmap f2 . fmap f1 $ res) init" $
    \(init:: [Bool]) (res:: ShowableEnvUnsafe [Bool] Integer) (Fun _ f1 :: Fun Integer Bool) (Fun _ f2 :: Fun Bool Integer) -> 
      let
        res' = showableToEnvUnsafe res 
      in
        runEnvUnsafe (fmap (f2 . f1) res') init == runEnvUnsafe (fmap f2 . fmap f1 $ res') init