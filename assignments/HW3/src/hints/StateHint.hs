-- This file is just for hints, examples, and test cases
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework
module StateHint where

import State


-- This is the example we may go over in lecture




push :: Integer -> State [Integer] ()
push x = do stack <- get
            put (x:stack)
            
pop :: State [Integer] Integer
pop = do stack <- get
         put (tail stack)
         return (head stack)

top :: State [Integer] Integer
top = do stack <- get
         return (head stack)


-- These do arithmetic on the stack

plus :: State [Integer] ()     
plus = do x <- pop
          y <- pop
          push (x+y)

mult :: State [Integer] ()
mult = do x <- pop
          y <- pop
          push (x*y)

          
                       
prog = do push 2        
          push 5       
          push 8       
          x <- pop      
          y <- pop       
          push (x - y)  
          mult          
          pop            


ex1 :: (Integer,[Integer])
ex1 = runState prog []


ex2 :: (Integer,[Integer])
ex2 = runState prog [4,5,6]

               
{-

State> ex1
(6,[])


State> ex2
(6,[4,5,6])


-}
