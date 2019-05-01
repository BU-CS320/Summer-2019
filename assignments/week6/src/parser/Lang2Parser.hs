module Lang2Parser where

import Lang2 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang2
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")


parser :: Parser Ast
parser = undefined

-- for repl testing, will only work if you implement Lang1's eval
data Lang2Out = ParseError | Result Integer [Integer] deriving (Show, Eq)

exec :: String -> Lang2Out
exec s = case parser s of
  Just (ast,"") -> case eval ast of
    (ls,i) -> Result i ls
  _  -> ParseError
