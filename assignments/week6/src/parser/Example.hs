module Example where

type Parser a = String  ->   Maybe (a, String)







--example :: String -> Maybe (Bool, String)
example :: Parser Bool
example ('T':'r':'u':'e': rest)     = Just (True, rest)
example ('F':'a':'l':'s':'e': rest) = Just (True, rest)
example _                           = Nothing