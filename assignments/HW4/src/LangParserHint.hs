-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework

module LangParserHint where

import Lang
import ParserMonad
import EnvUnsafe




-- you may want to structure you grammar like this:

keywords = ["if","then","else", "let", "in", "true","false"]

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = undefined

bools :: Parser Ast
bools = undefined

nil :: Parser Ast
nil = undefined

apps :: Parser Ast
apps = withInfix cons [("",App)] -- the tokens eat up all the spaces so we split on the empty string

cons :: Parser Ast
cons = 
 (do x <- orExpr
     token (literal ":")
     y <- cons
     return (Cons x y)) <|> orExpr
{-
  do x <- orExpr
     (do token (literal ":")
         y <- cons
         return (Cons x y)) <|> return x
-}
-- *LangParser> parse cons "1 : 4: true"
-- Just (1 : 4 : true,"")


orExpr :: Parser Ast
orExpr = withInfix andExpr [("||",Or)]

-- *LangParser> parse orExpr "true || false && 7"
-- Just (true || false && 7,"")
-- *LangParser> parse orExpr "true || false || 7"
-- Just (true || false || 7,"")
-- *LangParser> parse orExpr "true"
-- Just (true,"")

andExpr :: Parser Ast
andExpr = undefined

-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")

addSubExpr :: Parser Ast
addSubExpr = undefined

-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")

multDivExpr :: Parser Ast
multDivExpr = undefined

notExp :: Parser Ast
notExp = undefined

atoms:: Parser Ast
atoms = ints <|> bools <|>  nil <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars

-- *LangParser> parse atoms "111"
-- Just (111,"")
-- *LangParser> parse atoms "  true"
-- Just (true,"")

ifParser :: Parser Ast
ifParser = undefined

letParser :: Parser Ast
letParser = undefined

-- *LangParser> parse letParser "let x=3 in x+x"
-- Just (let x = 3 in x + x,"")


lambdaParser :: Parser Ast
lambdaParser = undefined

parens :: Parser Ast
parens = undefined

-- *LangParser> parse parser "(true)"
-- Just (true,"")
-- *LangParser> parse parser "let x = (if true and false then 3 else elsee) in x + x"
-- Just (let x = if true and false then 3 else elsee in x + x,"")




-- Some examples of weird stuff

ex = showPretty  (Mult (Var "a") (Or (Var "b") (Var "c"))) 0

ex1 = showPretty (Minus (Var "y") (Minus (App (ValBool True) (ValInt (-3))) (Mult (ValBool False) (ValBool False)))) 0

ex2 = showPretty (Cons (Var "z") (Not (Not (Plus (Mult (ValInt (-18)) Nil) (Not (ValInt 2)))))) 0

ex3 = "! ! (-18)"