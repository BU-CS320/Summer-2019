module StateTestType(State(..), ShowableState(..), showableToState) where

  import Test.Tasty.QuickCheck
  import State (State(..))

  data ShowableState s a = MkShowState (Fun s (a, s)) deriving Show

  -- createShowableState:: s -> a -> ShowableState s a
  -- createShowableState s a = ShowableState s a

  showableToState :: ShowableState s a -> State s a
  showableToState (MkShowState (Fun _ f)) = State f

  -- stateToShowable :: State s a -> ShowableState s a
  -- stateToShowable (State st) = ShowableState st

  instance (CoArbitrary s, Function s, Arbitrary a, Arbitrary s) => Arbitrary (ShowableState s a) where
    arbitrary = do fun <- arbitrary; return $ MkShowState fun 
    shrink (MkShowState fun) = [MkShowState fun' | fun' <- shrink fun]
  