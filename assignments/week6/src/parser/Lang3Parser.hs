module Lang3Parser where

import Data.Map (Map)-- for state
import qualified Data.Map as Map -- for State in tests

import Lang3 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang3
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")

parser :: Parser Ast
parser = undefined

-- for repl testing, will only work if you implement Lang3's eval
data Lang3Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean state keeping only the result
exec :: String -> Lang3Out
exec s = case parser s of
  Just (ast,"") -> case eval ast Map.empty of
    (Just i, _) -> Result i
    _  -> RuntimeError
  _  -> ParseError
