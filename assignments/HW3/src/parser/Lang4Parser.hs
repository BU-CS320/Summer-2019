module Lang4Parser where

import Data.Map (Map)-- for env
import qualified Data.Map as Map -- for env in tests

import Reader
import Lang4 (Ast(..), eval)

import ParserMonad


parser :: Parser Ast
parser = undefined


-- for repl testing
data Lang4Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean env keeping only the result
exec :: String -> Lang4Out
exec s = case (parse parser) s of
  Just (ast,"") -> Result $ runReader (eval ast) Map.empty
  _  -> ParseError



