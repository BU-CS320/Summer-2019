module Ast where

-- | the abstract syntax tree for the language
data Ast -- ...
--         deriving (Eq,Show)  -- helpful to use this during testing
--         deriving Eq 

instance Show Ast where
  -- display the ast in a readable way
  show ast = showPretty ast 0

  
  

-- | output the fully parenthesized statement
showFullyParen :: Ast  -- ^ The Ast to show
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParen = undefined

-- | provide a nice show with minimal parentheses
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty = undefined
