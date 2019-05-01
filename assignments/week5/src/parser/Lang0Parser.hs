module Lang0Parser where

import Lang0 hiding (test1)
import MyParserLib

-- parse the fully parenthesized Lang0 expression

literalIntParser :: Parser Ast
literalIntParser = token intParser
  `mapParser` \ i -> LiteralInt i

plusParser :: Parser Ast
plusParser = (token (literal "(")) +++ parser +++ (token (literal "+")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((((_, l), _), r), _) -> Plus l r

-- parse the pattern "(x)" into "x"
parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
                `mapParser` \ ((_, ast), _) -> ast
  
parser :: Parser Ast
parser = plusParser <||> literalIntParser <||> parseParens
  `mapParser` \ e -> case e of
    Right i -> i
    Left (Left i) -> i
    Left (Right i) -> i


-- for repl testing
data Lang0Out = ParseError | Result Integer deriving (Show, Eq)

exec :: String -> Lang0Out
exec s = case parser s of
  Just (ast,"") -> Result $ eval ast
  _  -> ParseError
  
-- *Lang0Parser> exec "(1+2)"
-- Result 3
-- *Lang0Parser> exec "7"
-- Result 7
-- *Lang0Parser> exec "(1   + (2  + 5))"
-- Result 8
-- *Lang0Parser> exec "1+2+3"
-- ParseError
