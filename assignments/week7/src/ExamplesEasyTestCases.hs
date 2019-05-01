module ExamplesAndEasyTests where

import Prelude


-- Some simple demonstrations/test cases for Lang0

e01 = (LiteralInt 6)

e02 = (Plus (LiteralInt 8) (LiteralInt 5))

e03 = (Sub (LiteralInt 8) (LiteralInt 5))

e04 = (Mult (LiteralInt 8) (LiteralInt 5))

e05 = (Mult (Plus (LiteralInt 5) (LiteralInt 3)) (Sub (LiteralInt 10) (LiteralInt 5)))


e06 = (Plus (Sub e02 e03) (Mult e04 e05))

{-   Here's how they should behave

Lang0> e01
6

Lang0> eval e01
6

Lang0> eval' e01
Identity 6

Lang0> e02
(8 + 5)

Lang0> eval e02
13

Lang0> eval' e02
Identity 13

Lang0> e03
(8 - 5)

Lang0> eval e03
3

Lang0> eval' e03
Identity 3

Lang0> e04
(8 * 5)

Lang0> eval e04
40

Lang0> eval' e04
Identity 40

Lang0> e05
((5 + 3) * (10 - 5))

Lang0> eval e05
40

Lang0> eval' e05
Identity 40

Lang0> e06
(((8 + 5) - (8 - 5)) + ((8 * 5) * ((5 + 3) * (10 - 5))))

Lang0> eval e06
1610

Lang0> eval' e06
Identity 1610

-}

-- Simple examples/easy test cases for Lang1


e11 = (Mult (Sub (LiteralInt 8) (LiteralInt 5)) (Plus (LiteralInt 4) (LiteralInt 2)))

e12 = (Div (LiteralInt 4) (LiteralInt 2))

e13err = (Div (LiteralInt 4) (LiteralInt 0))

e14 = (Mult (Sub (LiteralInt 8) (LiteralInt 5)) (Div e11 (Plus (LiteralInt 4) (LiteralInt 1))))

e15err = (Mult  (Div (Plus (LiteralInt 4) (LiteralInt 1)) (Sub (LiteralInt 8) (LiteralInt 8))) e11)


{-

Lang1> e11
((8 - 5) * (4 + 2))

Lang1> eval e11
Just 18

Lang1> e12
(4 / 2)

Lang1> eval e12
Just 2

Lang1> e13err
(4 / 0)

Lang1> eval e13err
Nothing

Lang1> e14
((8 - 5) * (((8 - 5) * (4 + 2)) / (4 + 1)))

Lang1> eval e14
Just 9

Lang1> e15err
(((4 + 1) / (8 - 8)) * ((8 - 5) * (4 + 2)))

Lang1> eval e15err
Nothing


-}

-- For Lang2

-- Some simple demonstrations/test cases

e21 =  (Mult (Sub (LiteralInt 8) (LiteralInt 5))
             (Plus (LiteralInt 4) (LiteralInt 2)))

e22 =  ((LiteralInt 4)                       `Separator`   
       ((Plus (LiteralInt 6) (LiteralInt 4)) `Separator`
       (Sub (LiteralInt 5) (LiteralInt 3))))
 
e23 =  ((Print (LiteralInt 4))                       `Separator`   
       ((Print (Plus (LiteralInt 6) (LiteralInt 4))) `Separator`
       (Sub (LiteralInt 5) (LiteralInt 3))))

e24 =  (e23 `Separator`
       (e22 `Separator`
       (e23 `Separator`
       ((Print (Print (LiteralInt 5))) `Separator`
       (e21)))))


{-

*Lang2> e21
((8 - 5) * (4 + 2))

*Lang2> runPrinterMonad $ eval e21
([],18)

*Lang2> e22
(4; ((6 + 4); (5 - 3)))

*Lang2> runPrinterMonad $ eval e22
([],2)

*Lang2> e23
(print(4); (print((6 + 4)); (5 - 3)))

*Lang2> runPrinterMonad $ eval e23
([4,10],2)

*Lang2> e24
((print(4); (print((6 + 4)); (5 - 3))); ((4; ((6 + 4); (5 - 3))); ((print(4); (print((6 + 4)); (5 - 3))); (print(print(5)); ((8 - 5) * (4 + 2))))))

*Lang2> runPrinterMonad $ eval e24
([4,10,4,10,5,5],18)

-}

