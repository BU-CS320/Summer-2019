module Lang4Parser where

import Data.Map (Map)-- for env
import qualified Data.Map as Map -- for env in tests

import Lang4 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang4
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")


parser :: Parser Ast
parser = undefined


-- for repl testing, will only work if you implement Lang3's eval
data Lang4Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean env keeping only the result
exec :: String -> Lang4Out
exec s = case parser s of
  Just (ast,"") -> case eval ast Map.empty of
    (Just i) -> Result i
    _  -> RuntimeError
  _  -> ParseError



