module Lang0Parser where

import Lang0 (Ast(..),eval)
import ParserMonad

-- this parses Lang0 expressions with associativity and precedence

parseAstInt :: Parser Ast
parseAstInt = token intParser
                `mapParser` (\ i -> LiteralInt i)


                
addSubEx = parse (addSub (LiteralInt 7)) "+ 3 + 4"
-- parse all the top level pluses and minuses
-- "-6" "+6" "+1*3" "+ 1 + 2 + 3"
addSub :: Ast -> Parser Ast
addSub left = do s <- token $ (literal "+") <||> (literal "-")
                 exp <- multEprOrParensOrInt
                 let res = case s of
                             Left _  -> left `Plus` exp
                             Right _ -> left `Sub` exp
                 (addSub res) <|> return res

                  
-- "5-6" "7+6" "2*4+1*3" "2*4 + 1 + 2 + 3" ...
addSubExpr :: Parser Ast
addSubExpr = do l <- multEprOrParensOrInt
                addSub l
                
-- parse all the top level multiplication
-- "*6" "*(1+3)" "* 1 * 2 * 3"
mults :: Ast -> Parser Ast
mults left = 
  do s <- (token $ literal "*")
     exp <- parensOrInt
     let res =  left `Mult` exp
     (mults res) <|> return res

	 
multEpr :: Parser Ast
multEpr = do l <- parensOrInt
             mults l

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast
			
			
parensOrInt :: Parser Ast
parensOrInt = parens <|> parseAstInt
			
multEprOrParensOrInt :: Parser Ast
multEprOrParensOrInt =  multEpr <|> parens <|> parseAstInt

parser :: Parser Ast
parser = addSubExpr <|> multEpr <|> parens <|> parseAstInt
			
			
-- for repl testing
data Lang0Out = ParseError | Result Integer deriving (Show, Eq)

exec :: String -> Lang0Out
exec s = case (parse parser) s of
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