module Reader where 

import Control.Monad(ap)

--This monad will form the plumbing for the evaluation function of lang4


data Reader env a = Reader (env -> a)

-- function that just runs the function contained in EnvUnsafe
runReader ::  (Reader e a) -> e -> a
runReader (Reader f) env = f env


-- a way to easily get the entire environment (for instance in do notation)
ask :: Reader e e
ask = Reader $ \ env -> env


instance Functor (Reader e) where
  -- fmap :: (a -> b) -> Reader e a -> Reader e b
  fmap f (Reader g) = undefined
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (Reader e) where
  pure = return
  (<*>) = ap

instance Monad (Reader e) where
  --return :: a -> Reader e a
  return a = undefined

  --(>>=) :: Reader a -> (a -> Reader e b) -> Reader e b
  (Reader g) >>= f = undefined

  -- make sure your implementation follows the Monad laws


-- "local" runs functions under a modified local environment
local :: (r -> r) -> Reader r a -> Reader r a
local changeEnv comp  = Reader (\e -> runReader comp (changeEnv e) )

-- technical note: 
-- this is modled after the Reader Monad in http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html