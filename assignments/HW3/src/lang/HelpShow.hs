module HelpShow where


-- | helper function to 'showPretty'
-- It determines whether to parenthesize current expression 
-- based on the precedence level of current operator and outer operator.
-- The smaller precedence level means the opearator have higher precedence.
-- If the inner expression (current expression) has lower precedence (larger precedence level),
-- then the inner expression (current expression) will need to be parenthesized.
--
-- Example: 
-- @
--    Mult 
--      (Plus (LiteralInt 1) (Literal Int 2))
--      LiteralInt 2
-- @
-- The Plus inside need to be parenthesized, 
-- Because the inner expression (current expression) Plus 
-- has lower precedence than the outer expression Mult.

parenthesize :: Integer -- ^ the precedence level of outer expression
              -> Integer -- ^ the precedence level of the current expression
              -> String -- ^ string representation current expression
              -> String -- ^ the properly (not necessarily fully) parenthesized current expression

parenthesize outerLevel curLevel showExp 
  | outerLevel < curLevel = "(" ++ showExp ++ ")"
  | otherwise             =        showExp

