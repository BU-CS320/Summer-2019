module Lang4Parser where

import Data.Map (Map)-- for env
import qualified Data.Map as Map -- for env in tests

import Lang4 (Ast(..), eval)

import ParserMonad


parser :: Parser Ast
parser = undefined -- ungraded bonus


-- for repl testing
data Lang4Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean env keeping only the result
exec :: String -> Lang4Out
exec s = case (parse parser) s of
  Just (ast,"") -> case eval ast Map.empty of
    (Just i) -> Result i
    _  -> RuntimeError
  _  -> ParseError



