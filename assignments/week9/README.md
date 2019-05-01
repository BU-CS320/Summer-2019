# Week9 Due 4/~~2~~ 3
extention in [@1102](https://piazza.com/class/jr9fgrf7efv7j0?cid=1102)

## Analytical  Part
There is no analytical homework this week. 
## Code Part
### Instructions
* To get the assignment run ```git pull upstream master``` in your homework directory
* `cd` into the `week9` directory, run `cabal new-repl`
* Fill in the bodies of the undefined functions and data
* DO NOT CHANGE THE TYPE SIGNATURES!

### Notes
* A set of examples and simple test cases for the lambda parser have been posted on the class web site: 
[LambdaCalcParserExamples.hs](http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/LambdaCalcParserExamples.hs). 
* A set of examples and simple test cases for the lambda calculus implementation (capture-avoiding style) is posted on the class web site:
[LambdaCalcImpTest.hs](http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/LambdaCalcImpTest.hs)
* Comprehensive automated tests have not been posted yet
* As described in the hints, `show` must write lambdas in the Haskell style.  For instance the lambda term `(λx.x x) λx.x x` should show as `(\x -> x x) \x -> x x`.  The standard precedence and associativity rules apply: all you need to know is that function application has higher precedence than abstraction (i.e., the operator -> ). 
* The parser should handle arbitrary lambda terms in the style above. The parser will be graded, though there are additional features you can add that are optional.


### Submit (similar to [week1](../week1))
1. run the tests by running ```cabal new-test``` 
1. run ```git status``` to make sure git is ok
1. run ```git commit -a -m "please don't remove these instructions"``` to make a commit to your laptop
1. run ```git pull upstream master``` to get the latest tests
1. run the tests by running ```cabal new-test``` 
1. run ```git push``` to submit your commit to your private gitHub account
1. check that you can see your solutions on the website for your private repo

### REPL hints
* `:load` or `:l` will change the module you are inspecting
* `:reload` or `:r` will reload the file.  Do this often!
* `:type` or `:t` will tell you the type of an expression
* `:quit` or `:q` will leave the repl
