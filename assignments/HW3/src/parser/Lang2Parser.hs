module Lang2Parser where

import Lang2 (Ast(..), eval)
import PrinterMonad(runPrinterMonad)
import ParserMonad



parser :: Parser Ast
parser = undefined

-- for repl testing
data Lang2Out = ParseError | Result Integer [Integer] deriving (Show, Eq)

exec :: String -> Lang2Out
exec s = case (parse parser) s of
  Just (ast,"") -> case runPrinterMonad (eval ast) of
    (ls,i) -> Result i ls
  _  -> ParseError
