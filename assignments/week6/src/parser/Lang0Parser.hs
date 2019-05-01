module Lang0Parser where

import Lang0 (Ast(..),eval)
import MyParserLib

-- parse the fully parenthesized Lang0 expression

int1 = "1234"
int2 = "  0001234"
int3 = "-2324"

literalIntParser :: Parser Ast
literalIntParser = token intParser
  `mapParser` (\ i -> LiteralInt i)

ad1 = "(1+3)"
ad2 = "(1+-3)"
ad3 = "  (  1+  3) "

plusParser :: Parser Ast
plusParser = token (literal "(") +++ parser +++ token (literal "+") +++ parser +++ token (literal ")")
  `mapParser` (\ ((((_ , l),_),r),_) -> Plus l r)


pex1 = "(1)"
pex11 = "  (  1  )  "
pex2 = "((1))"
pex3 = "((((1))))"
pex4 = "(2+5)"
-- parse the pattern "(x)" into "x"
parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
  `mapParser` (\ ((_,ast),_) -> ast)

  
parser :: Parser Ast
parser = literalIntParser <||> plusParser <||> parseParens 
  `mapParser` (\ e -> case e of
    Left (Left ast) -> ast
    Left (Right ast) -> ast
    Right ast -> ast)



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
