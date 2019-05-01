module Lang1Parser where

import Lang1 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang1
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")

parser :: Parser Ast
parser = undefined


-- for repl testing, will only work if you implement Lang1's eval
data Lang1Out = ParseError | DivZeroErro | Result Integer deriving (Show, Eq)

exec :: String -> Lang1Out
exec s = case parser s of
  Just (ast,"") -> case eval ast of
    Just i -> Result i
    _      -> DivZeroErro
  _  -> ParseError
