module Lang1 where


data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Div Ast Ast
  | Sub Ast Ast
  | Mult Ast Ast

-- hint: use do notation from the built in monad on Maybe
eval :: Ast -> Maybe Integer
eval (LiteralInt i) = return i

eval (Div l r) = 
  do l' <- eval l
     r' <- eval r
	 if (r' == 0)
	 then Nothing
	 else return undefined
eval (Plus l r) = 
     case (eval l) of
     Nothing -> Nothing
     Just l' ->
       case (eval r) of
         Nothing -> Nothing
         Just r' -> Just (l' + r')

--     (eval l) >>= (\ l' -> 
--     (eval r) >>= (\ r' -> 
--     return (l' + r')))

--  do l' <- eval l
--     r' <- eval r
--     return (l' + r')
     
ex = (LiteralInt 2) `Plus` (LiteralInt 7)
     
-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
  show (l `Sub` r) = "(" ++ (show l) ++ " - " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
