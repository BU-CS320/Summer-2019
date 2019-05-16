module Lang1Parser where

import Lang1 (Ast(..), eval)
import ParserMonad


parser :: Parser Ast
parser = undefined 


-- for repl testing
data Lang1Out = ParseError | DivZeroErro | Result Integer deriving (Show, Eq)

exec :: String -> Lang1Out
exec s = case (parse parser) s of
  Just (ast,"") -> case eval ast of
    Just i -> Result i
    _      -> DivZeroErro
  _  -> ParseError
