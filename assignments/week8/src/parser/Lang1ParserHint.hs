-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module Lang1ParserHint where

import Lang1 (Ast(..),eval)
import ParserMonad



{- 
in lecture you derived the grammar for Lang1.  It looked like 

	S -> E

	E -> E + T
	E -> E - T
	E -> T

	T -> T * F
	T -> T / F
	T -> F

	F -> Integers
	F -> ( E )

We will convert this presentation to an outline of code below.

When working on the whiteboard it is conventional to use short names. When writing code it is conventional to use longer human readable names.

-}


-- Integers in the grammar
parseAstInt :: Parser Ast
parseAstInt = undefined

-- ( E ) in the grammar
parens :: Parser Ast
parens = undefined

-- "F" in the grammar, could also call it parensOrInt
factor = parseAstInt <|> parens 

-- we need to deal with the left recursive case of "T" together
--	T -> T * F
--  T -> T / F
--  T -> F

-- this will parse all the top level multiplication and division, given what was parsed on the left
multDivs :: Ast -> Parser Ast
multDivs left = 
  do s <- token ((literal "*") <||> (literal "/"))
     ast <- factor
     let res = case s of
                 Left _  -> undefined
                 Right _ -> undefined
     (multDivs res) <|> return res -- keep doing this as much as possible

-- run these in your repl to tests
exMultDivs1 =  parse (multDivs (LiteralInt 0)) "*6"
exMultDivs2 =  parse (multDivs (LiteralInt 6)) "  /  3"
exMultDivs3 =  parse (multDivs (LiteralInt 1)) "  * 10 / 5 * 4 /2"
	 
-- finally we combine all case of "T" together
--	T -> T * F
--  T -> T / F
--  T -> F
term :: Parser Ast
term =
  do l <- factor -- every term starts with a factor
     mults l <|> return l
	 
-- run these in your repl to test
exTerm1 =  parse term "100"
exTerm2 =  parse term "0*6"
exTerm3 =  parse term "6  /  3"
exTerm4 =  parse term " 1 * 10 / 5 * 4 /2"



-- we need to deal with the left recursive case of "E" together
--  E -> E + T
--  E -> E - T

addSubs :: Ast -> Parser Ast
addSubs left = 
  do s <- token ((literal "+") <||> (literal "-"))
     ast <- term
     let res = case s of
                 Left _  -> left `Plus` ast
                 Right _ -> left `Sub` ast
     (addSubs res) <|> return res

-- run these in your repl to test
exAddSubs1 =  parse (addSubs (LiteralInt 6)) "  -  3"
exAddSubs2 =  parse (addSubs (LiteralInt 100)) " - 6 - 4"
exAddSubs3 =  parse (addSubs (LiteralInt 1)) "  + 10 - 5 + 4 -2"

addOrSubOrMultOrDivOrParensOrInt :: Parser Ast
addOrSubOrMultOrDivOrParensOrInt =
  do l <- term
     addSubs l

	 
-- finally we combine all case of "E" together
--  E -> E + T
--  E -> E - T
--  E -> T
expresion :: Parser Ast
expresion = addOrSubOrMultOrDivOrParensOrInt <|> term


-- "S" in the grammar
parser = expresion

