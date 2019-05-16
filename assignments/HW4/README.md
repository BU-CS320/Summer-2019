# This assignment is a DRAFT!
that means some problems may change slightly and the tests are incomplete.  Do not edit unless you are comfortable with merge conflicts

* tests need to be hooked together
* instructions need cleaning up



# HW4
Due xx


## Analytical  Part
Posted on the class web site, due Thursday 4/11:  [Last HW](http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/hwWeek10.pdf)
## Code Part
This week we will write our largest language so far.  It is a functional language that has Bools, Integers, and Lists.
* You will need to write the monadic plumbing in EnvUnsafe.hs. Note that this is a combination of the Reader Monad and the Ok/Maybe Monad.  You can pretty much write this by referring to those previous monads and paying attention to the types. 
* You will need to write an evaluation function in Lang.hs
* you will need to write a parser in LangParser.hs

### Instructions
* To get the assignment run ```git pull upstream master``` in your homework directory
* `cd` into the `week9` directory, run `cabal new-repl`
* Fill in the bodies of the undefined functions and data
* DO NOT CHANGE THE TYPE SIGNATURES!

### Notes
* Some simple examples and easy tests cases for the parser have been posted on the class web site: [LangParserEasyTests.hs](http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/LangParserEasyTests.hs)
* Automated tests for the parser have been posted; automated tests for the evaluation function soon....
* See [hints video](https://www.youtube.com/watch?v=QUBUPId5WRQ&t=7s) posted last semester.
* Some comments on the parser have been uploaded to Snyder's YT channel: [YT](https://www.youtube.com/watch?v=Pc6Lz9MkWEU&t=14s) 
* You can choose your own error messages as long as they are reasonable.



### Starting Out


Before you begin anything experiment with the Ast we have given you ```:load Lang``` and try 5-10 different ASTs so you get a sense of how the language "looks",
The show instance is hooked up to a pretty printer we provide that matches the exact syntax that you will eventually parse.

for example (you will not be able to run them yet but you should think about what the answer is):
```
*Lang> Plus (ValInt 2) (ValInt 3)
2 + 3
*Lang> run $ Plus (ValInt 2) (ValInt 3)
Ok (I 5)
*Lang> And (ValBool True) (ValBool False)
true && false
*Lang> run $ And (ValBool True) (ValBool False)
Ok (B false)
*Lang> And (ValBool True) (ValInt 3)
true && 3
*Lang> run $ And (ValBool True) (ValInt 3)
Error "expected a bool but found an Int!"
*Lang> ((Lam "x" (Lam "y" ( (Var "x") `Plus` (Var "y")))) `App` (ValInt 7)) `App` (ValInt 4)
(\ x -> \ y -> x + y) 7 4
*Lang> run $ ((Lam "x" (Lam "y" ( (Var "x") `Plus` (Var "y")))) `App` (ValInt 7)) `App` (ValInt 4)
Ok (I 11)
```


### Submit (similar to [week1](../week1))

### REPL hints
* `:load` or `:l` will change the module you are inspecting
* `:reload` or `:r` will reload the file.  Do this often!
* `:type` or `:t` will tell you the type of an expression
* `:quit` or `:q` will leave the repl


