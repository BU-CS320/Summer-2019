# HW6 Due 3/19


## Analytical  Part
Due Wed 3/20 [@861](https://piazza.com/class/jr9fgrf7efv7j0?cid=861)
available [here](http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/hw06.pdf)

## Code Part
You are only responsible for the following files
BareBonesLast, PrinterMonad, Lang1, Lang2.
We have included a number of other files you can use to test (the parsers), that we plan to assign in future weeks (Lang3, Lang4), and that give hints (Lang0)

### Instructions
* To get the assignment run ```git pull upstream master``` in your homework directory
* `cd` into the `week7` directory, run `cabal new-repl`
* Fill in the bodies of the undefined functions and data
* DO NOT CHANGE THE TYPE SIGNATURES!

### Notes
* Some preliminary examples and easy test cases are available here:  http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/ExamplesEasyTestCases.hs
* All tests are out.
    - if you encounter a problem saying that `cabal.exe: Cannot test the package because none of the components are available to build`, you need to run `cabal new-test --enable-test`.
* A hint file has been added [Lang2Hint.hs](src/lang/Lang2Hint.hs)
* For `Monad List`, you may find `(++)` and `concat` defined in [HW2](https://github.com/BU-CS320/Spring-2019/blob/master/assignments/week3/src/Hw02.hs#L40) helpful, to make this easy you can change `import Prelude hiding (List(..), Pair(..),Maybe(..),Either(..),)` to `import Prelude hiding (List(..), Pair(..),Maybe(..),Either(..),(++),concat)`.
* Watch the videos: [Summary of first lecture on Monads](https://www.youtube.com/watch?v=i8E0G9S3ty0), [Monad Lecture Code 1 walk through](https://www.youtube.com/watch?v=YKgVebCiDDg0), [Monad Lecture Code 2 Walk Through](https://www.youtube.com/watch?v=45eQyaKUxXY), and others on [Wayne Snyder's channel](https://www.youtube.com/channel/UCfSqNB0yh99yuG4p4nzjPOA)

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
