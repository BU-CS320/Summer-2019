-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module Lang0ParserFast where

import Lang0 (Ast(..),eval, showPretty)
import ParserMonad


-- the parser given in Lang0Parser had an inefficiency bug,
-- pointed out in https://piazza.com/class/jr9fgrf7efv7j0?cid=1023
-- under our grading plan it solution would have received full credit, but fixing the bug is instructive

-- fist find the simplest test cases for the Lang0Parser
slow = parse parser "(((((((0)))))))"
veryslow = parse parser "(((((((((0)))))))))"


-- the solution is to make the parser code more closely match the CFG derivation


-- this parses Lang0 expressions with associativity and precedence

parseAstInt :: Parser Ast
parseAstInt = token intParser
                `mapParser` (\ i -> LiteralInt i)

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


