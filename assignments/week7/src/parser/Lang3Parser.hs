module Lang3Parser where

import Data.Map (Map)-- for state
import qualified Data.Map as Map -- for State in tests

import Lang3 (Ast(..), eval)
import ParserMonad


parser :: Parser Ast
parser = undefined -- ungraded bonus

-- for repl testing
data Lang3Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean state keeping only the result
exec :: String -> Lang3Out
exec s = case (parse parser) s of
  Just (ast,"") -> case eval ast Map.empty of
    (Just i, _) -> Result i
    _  -> RuntimeError
  _  -> ParseError
