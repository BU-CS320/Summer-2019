-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module Lang2ParserHint where

import Lang2 (Ast(..),eval)
import ParserMonad



-- we asked that separators parse in a right associative way.  This turns out to be easier than parsing left associative terms.

seporators :: Parser Ast
seporators =
  do token $ literal ";"
     (seporatorExpr <|> parseAstInt)


seporatorExpr = 
  do l <- parseAstInt 
     r <- seporators
     return $ Separator l r
	 

exSeporators1 = parse seporators "; 1"
exSeporators2 = parse seporators "; 1; 2"
exSeporators3 = parse seporators "; 1; 2    ;3"

exSeporatorExpr1 = parse seporatorExpr "0 ; 1"
exSeporatorExpr = parse seporatorExpr "0 ; 1 ; 2 ; 3"

parseAstInt :: Parser Ast
parseAstInt = token intParser
                `mapParser` (\ i -> LiteralInt i)