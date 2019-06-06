-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module Lang0Parser where

import Lang0 (Ast(..),eval, showPretty)
import ParserMonad


-- this parses Lang0 expressions with associativity and precedence

parseAstInt :: Parser Ast
parseAstInt = 
  do i <- (token intParser)
     return $ LiteralInt i

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast


parensOrInt :: Parser Ast
parensOrInt = parens <|> parseAstInt


-- parse all the top level multiplication
-- "*6" "*(1+3)" "* 1 * 2 * 3"
mults :: Ast -> Parser Ast
mults left = 
  do s <- (token $ literal "*")
     exp <- parensOrInt
     let res =  left `Mult` exp
     (mults res) <|> return res

     
multOrParensOrInt :: Parser Ast
multOrParensOrInt =
  do l <- parensOrInt
     (mults l <|> return l) -- this is the fastest way, it avoids redoing any work
     
exMultOrParensOrInt1 = parse multOrParensOrInt "1"
exMultOrParensOrInt2 = parse multOrParensOrInt "1*2*3"
exMultOrParensOrInt3 = parse multOrParensOrInt "1+1"  -- shouldn't work!
-- ...


-- parse all the top level pluses and minuses
-- "-6" "+6" "+1*3" "+ 1 + 2 + 3"
addSub :: Ast -> Parser Ast
addSub left =
  do s <- token $ (literal "+") <||> (literal "-")
     exp <- multOrParensOrInt
     let res = case s of
                 Left _  -> left `Plus` exp
                 Right _ -> left `Sub` exp
     (addSub res) <|> return res

-- "5-6" "7+6" "2*4+1*3" "2*4 + 1 + 2 + 3" ...
addOrSubOrMultOrParensOrInt:: Parser Ast
addOrSubOrMultOrParensOrInt =
  do l <- multOrParensOrInt
     (addSub l <|> return l)

exAddOrSubOrMultOrParensOrInt1 = parse addOrSubOrMultOrParensOrInt "1"
exAddOrSubOrMultOrParensOrInt2 = parse addOrSubOrMultOrParensOrInt "1*2*3"
exAddOrSubOrMultOrParensOrInt3 = parse addOrSubOrMultOrParensOrInt "1+1+1" 
exAddOrSubOrMultOrParensOrInt4 = parse addOrSubOrMultOrParensOrInt "1*3 + 1 + 9*9*9" 


parser :: Parser Ast
parser = addOrSubOrMultOrParensOrInt
            
            
-- for repl testing
data Lang0Out = ParseError | Result Integer deriving (Show, Eq)

exec :: String -> Lang0Out
exec s = case (parse parser) s of
  Just (ast,"") -> Result $ eval ast
  _  -> ParseError


