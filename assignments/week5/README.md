# HW4 Due 2/25
Due Monday at midnight to accommodate your midterm

## Analytical  Part
Posted [@659](https://piazza.com/class/jr9fgrf7efv7j0?cid=659), availible [here](http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/hw04.pdf)
## Code Part
for Monday you are only resposnable for
* BareBonesRevenge.hs
* TypeclassProblems.hs
### Instructions
* To get the assignment run ```git pull upstream master``` in your homework directory
* `cd` into the `week5` directory, run `cabal new-repl`
* Fill in the bodies of the undefined functions and data
* DO NOT CHANGE THE TYPE SIGNATURES!

### Notes
* Some tests are posted
  * Before running tests you must define `DayOfTheWeek` with `deriving Show`
  * Before running tests you must define all the instances in `BareBonesRevenge` you may start will silly incorrect implementations
    * `  _ == _ = False`
    * `  _ <= _ = False`
    * `  fmap _ _ = undefined`
* Posted hints in [TypeclassProblemsHints.hs](src/TypeclassProblemsHints.hs)
* Please `deriving Show` on your `DayOfTheWeek`
* Because of the way Ord depends on Eq, you will get errors if you have `instance Eq a => Eq ...` but no `instance Ord a => Ord ...`, this is just haskell trying to be helpful
* `instance (AllTheThings a, AllTheThings b) => AllTheThings (a,b)` is harder than expected, we plan to post a hint soon
* For `instance HasExample [a]` remember that lists can be empty
* We plan to post a video on the parsing part
* "lexographicly" should have been spelled "[lexicographically](https://en.wikipedia.org/wiki/Lexicographical_order)"

### Submit (similar to [week1](../week1))
1. run the tests by running ```cabal new-test``` 
1. run ```git status``` to make sure git is ok
1. run ```git commit -a -m "haskell is fun"``` to make a commit to your laptop
1. run ```git pull upstream master``` to get the latest tests
1. run the tests by running ```cabal new-test``` 
1. run ```git push``` to submit your commit to your private gitHub account
1. check that you can see your solutions on the website for your private repo

### REPL hints
* `:load` or `:l` will change the module you are inspecting
* `:reload` or `:r` will reload the file.  Do this often!
* `:type` or `:t` will tell you the type of an expression
* `:quit` or `:q` will leave the repl
