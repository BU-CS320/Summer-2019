module Lang0 where

import HelpShow

import Data.Functor.Identity 

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Sub Ast Ast
  | Mult Ast Ast
  deriving Show



eval :: Ast -> Integer
eval (LiteralInt i) = i
eval (x `Plus` y)   = (eval x) + (eval y)
eval (x `Sub` y)    = (eval x) - (eval y)
eval (x `Mult` y)   = (eval x) * (eval y)

-- the monadic way. silly here, but a strong hint for how to do the other languages.
eval' :: Ast -> Identity Integer
eval' (LiteralInt i) = return i
eval' (x `Plus` y) =
  do x' <- eval' x
     y' <- eval' y
     return (x' + y')
eval' (x `Sub` y) =
  do x' <- eval' x
     y' <- eval' y
     return (x' - y')
eval' (x `Mult` y) =
  do x' <- eval' x
     y' <- eval' y
     return (x' * y')

--  for all ast,  eval ast == runIdentity (eval' ast)


-- | show the ast in fully parenthesized way
-- Every expression will be parenthesized 
-- except for LiteralInt
-- 
-- Example:
-- > showFullyParen (LiteralInt 5) => 5
-- > showFullyParen ((LiteralInt 5) `Plus` (LiteralInt 3)) => (5 + 3)
showFullyParen :: Ast -> String
showFullyParen (LiteralInt i) = show i
showFullyParen (l `Plus` r) = "(" ++ (showFullyParen l) ++ " + " ++  (showFullyParen r) ++ ")"
showFullyParen (l `Sub` r) = "(" ++ (showFullyParen l) ++ " - " ++  (showFullyParen r) ++ ")"
showFullyParen (l `Mult` r) = "(" ++ (showFullyParen l) ++ " * " ++  (showFullyParen r) ++ ")"



-- | This function prints your ast with minimal parenthesis
--
-- Example 1:
-- @
--    Mult 
--      (Plus (LiteralInt 1) (Literal Int 2))
--      LiteralInt 3
-- @
-- will be printed as 
-- >  (1 + 2) * 3
-- 
-- Example 2:
-- @
--    Plus 
--      (Plus (LiteralInt 1) (Literal Int 2))
--      LiteralInt 3
-- @
-- will be printed as
-- > 1 + 2 + 3
--
-- Example 3:
-- @
--    Plus 
--      LiteralInt 3
--      (Plus (LiteralInt 1) (Literal Int 2))
-- @
-- will be printed as
-- > 3 + (1 + 2)
showPretty :: Ast -> Integer -> String
showPretty (LiteralInt i) _ = show i
showPretty (l `Mult` r)   d = parenthesize d 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0) )
showPretty (l `Plus` r)   d = parenthesize d 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
showPretty (l `Sub` r)    d = parenthesize d 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2) )

-- show pretty syntax
--instance Show Ast where
--  show x = showPretty x 100
