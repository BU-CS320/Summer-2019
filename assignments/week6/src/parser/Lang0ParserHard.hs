module Lang0ParserHard where

import Lang0 (Ast(..),eval)
import MyParserLib

-- this parses unparenthesized Lang 0 expressions, we will talk about how to do this generally in a couple weeks
-- we are including for reference if you want to look ahead

parseAstInt :: Parser Ast
parseAstInt = token intParser
                `mapParser` (\ i -> LiteralInt i)

-- parse the pattern "+ x + y + z" into [x,y,z]
pluses :: Parser [Ast]
pluses = (rep ( token ((literal "+") +++ parseIntOrParens)
            `mapParser` \ (_, ast) -> ast) )


-- parse the pattern "(x)" into "x"
parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
                `mapParser` \ ((_, ast), _) -> ast

-- parse patterns like "(x)" or "123"
parseIntOrParens :: Parser Ast
parseIntOrParens = parseAstInt <||> parseParens
                     `mapParser` \ ast -> case ast of
                                            Left a  -> a
                                            Right a -> a

-- could be a fold
combinePlus :: (Ast, [Ast]) -> Ast
combinePlus (a, []) = a
combinePlus (a, (h:t)) =  a `Plus` (combinePlus (h, t))

-- parse everything
parser :: Parser Ast
parser = parseIntOrParens +++ pluses `mapParser` combinePlus

-- for repl testing
data Lang0Out = ParseError | Result Integer deriving (Show, Eq)

exec :: String -> Lang0Out
exec s = case parser s of
  Just (ast,"") -> Result $ eval ast
  _  -> ParseError

-- *Lang0ParserHard> exec "1+2+3+4"
-- Result 10
-- *Lang0ParserHard> exec "1+2+3   +4"
-- Result 10
-- *Lang0ParserHard> exec "1+  (2+3 )  +4"
-- Result 10
-- *Lang0ParserHard> exec "1+  (2+3 )  +-4"
-- Result 2
-- *Lang0ParserHard> exec "1+  (2+3 )  +   -4"
-- Result 2
-- *Lang0ParserHard> exec "1+  (2+3 )  +   - 4"
-- ParseError
-- *Lang0ParserHard> exec "1+  (2+3 )  +   -4"
-- Result 2