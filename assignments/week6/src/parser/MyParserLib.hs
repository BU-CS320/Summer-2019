module MyParserLib where

import Data.Char

type Parser a = String -> Maybe (a, String)


-- parse one thing, if that works then parse the other thing
--       Parser a -> Parser b -> String -> Maybe ((a,b), String)
(+++) :: Parser a -> Parser b -> Parser (a,b)
(pa +++ pb) input = case pa input of
  Nothing -> Nothing
  Just (a, input2) -> case pb input2 of
    Nothing -> Nothing
    Just (b, input3) -> Just ((a,b), input3)


-- parse exactly a string, return that string
-- (in the Hutton book this function is called string)
literal :: String -> Parser String
literal "" input = Just ("", input)
literal str "" = Nothing
literal (h:t) (hi:ti) = if h == hi
                        then case literal t ti of
                              Nothing -> Nothing
                              Just (s,rest) -> Just (h:s,rest) -- bug
                        else Nothing


simpleParser = (literal "abc") +++ (literal "def")

-- *MyParserLib> simpleParser "abcdef"
-- Just (("abc","def"),"")
-- *MyParserLib> simpleParser "abcdefgh"
-- Just (("abc","def"),"gh")
-- *MyParserLib> simpleParser "abcd"
-- Nothing



-- parse natural numbers, like "123", or "000230000"
natParser :: Parser Integer
natParser ""    = Nothing    
natParser (h:t) = if isDigit h    
                  then natParserHelper 0 (h:t)
                  else Nothing
  where
    -- accumulate a number while parsing
    natParserHelper :: Integer -> Parser Integer    
    natParserHelper n ""    = Just (n, "")    
    natParserHelper n (h:t) = if isDigit h    
                              then let next = (read [h] :: Integer)    
                                   in natParserHelper (n*10 + next) t    
                              else Just (n, h:t)
                       
posFloatParser' = natParser +++ (literal ".") +++ natParser

-- *MyParserLib> posFloatParser' "1234.5678 and then some stufff"
-- Just (((1234,"."),5678)," and then some stufff")

-- not the best output right?

-- change the output of an existing parser
mapParser :: Parser a -> (a->b) -> Parser b
mapParser pa f input = case pa input of
  Nothing -> Nothing
  Just (a, input2) -> Just (f a,input2)


-- think, could Parsers be functors?  Do they obay the functor laws?  Do they fit any other abstractions?

-- a buggy implementation for demo purposes
numDigits:: Integer -> Integer
numDigits n = if n <= 9
              then 1
              else (numDigits (n `quot` 10)) + 1

numDigitsExample1 = (numDigits 999) == 3
numDigitsExample2 = (numDigits 1000) == 4
numDigitsExampleBug = (numDigits 0001000) == 4 --here's the bug!

posFloatParser =  (natParser +++ (literal ".") +++ natParser)
  `mapParser` (\ ((whole, _), fract) -> (fromIntegral whole) + (fromIntegral fract)/(10 ^^ (numDigits fract) ))

-- *MyParserLib> posFloatParser "1234.5678 and then some stufff"
-- Just (1234.5678," and then some stufff")
-- *MyParserLib> posFloatParser "1234 and then some stufff"
-- Nothing


--try to parse a, if that doesn't work try to pars b
(<||>) :: Parser a -> Parser b -> Parser (Either a b)
(pa <||> pb) input = case pa input of
  Just (a,in2) -> Just (Left a, in2)
  Nothing -> case pb input of
    Nothing -> Nothing
    Just (b,in2) -> Just (Right b, in2)


-- might be useful in the HW
intParser  :: Parser Integer
intParser = ((literal "-" +++ natParser) <||> natParser)
  `mapParser` (\ res -> case res of
     Left (_ , n) -> -n
     Right n -> n
     )



floatParser =  (intParser +++ (literal ".") +++ natParser) 
  `mapParser` (\ ((whole, _), fract) -> (fromIntegral whole) + (fromIntegral fract)/(10 ^^ (numDigits fract) ))

floatOrInt  = floatParser <||> intParser

-- *MyParserLib> floatOrInt "1000 and stuff"
-- Just (Right 1000," and stuff")
-- *MyParserLib> floatOrInt "1000.999 and stuff"
-- Just (Left 1000.999," and stuff")



-- take a parser and repeatedly parse as much as possible into a list
-- (in the Hutton book this function is called many)
rep :: Parser a -> Parser ([a])
rep pa input = case pa input of
  Nothing -> Just ([], input)
  Just (a, input1) -> case rep pa input1 of
    Nothing -> Nothing
    Just (als, input2) -> Just (a:als, input2)
    

numbers' = rep (floatOrInt +++ literal ",")

-- *MyParserLib> numbers' "1,2.34,5,"
-- Just ([(Right 1,","),(Left 2.34,","),(Right 5,",")],"")
-- *MyParserLib> numbers' "1, 2.34, 5,"  -- fails at first unexpected thing (in this case a space)
-- Just ([(Right 1,",")]," 2.34, 5,")


--parse spaces, throw them away
spaces :: Parser ()
spaces = (rep ((literal " ") <||> (literal ['\t']))) `mapParser` (\ _ -> ())

-- *MyParserLib> spaces "       and stuf"
-- Just ((),"and stuf")

-- eat the spaces before or after a parser
-- (equivalent to the Hutton book)
token :: Parser a -> Parser a
token pa = (spaces +++ pa +++ spaces) `mapParser` \ ((_, a), _) -> a

numbers = rep ((token floatOrInt) +++ literal ",")
-- *MyParserLib> numbers "1, 2.34  , 5,"
-- Just ([(Right 1,","),(Left 2.34,","),(Right 5,",")],"")


-- parse a single letter
letterParser :: Parser Char
letterParser "" = Nothing
letterParser (head:tail) = if isAlpha head
                           then Just (head, tail)
                           else Nothing


-- parse what we will consider a good variable name (for now)
-- this will be helpful for parsing Lang 3 and 4
varParser :: Parser String
varParser = (letterParser +++ rep letterParser ) `mapParser` (\ (h, tail) -> h:tail)



-- note if there are other ways you would like to combine parsers, feel free to add them here.