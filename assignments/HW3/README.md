# This assignment is a DRAFT!
that means some problems may change slightly and the tests are incomplete.  Do not edit unless you are comfortable with merge conflicts

* tests need to be hooked together
* instructions need cleaning up



# HW3
Due xx

extendeing the homework becuse of late tests [@974](https://piazza.com/class/jr9fgrf7efv7j0?cid=974)
[@1013](https://piazza.com/class/jr9fgrf7efv7j0?cid=1013)

## Analytical  Part
http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/hw.week8.pdf

Hints on the [YT channel](https://www.youtube.com/watch?v=GpqSS075bQM&t=30s)

## Code Part
You are responsible for State, Lang3, Reader, Lang 4, and all the parsers.
### Instructions
* To get the assignment run ```git pull upstream master``` in your homework directory
* `cd` into the `week8` directory, run `cabal new-repl`
* Fill in the bodies of the undefined functions and data
* DO NOT CHANGE THE TYPE SIGNATURES!

### Notes
* Automated tests have been posted.
* Wayne posted a video on his [YT channel](https://www.youtube.com/watch?v=ki6ZLeper70).  The debugging hints in this [video](https://youtu.be/fJ1QPEfmXi8) still apply.
* In [@1023](https://piazza.com/class/jr9fgrf7efv7j0?cid=1023) an efficeincy bug in my example code was pointed out. It is fixed in [Lang0ParserFast.hs](https://github.com/BU-CS320/Spring-2019/blob/master/assignments/week8/src/parser/Lang0ParserFast.hs).  please take a look if you have timeout issues.
* As before, Lang3 should evaluate left to right, and assignment returns the value assigned.  For instance `(x := 2) + x` should eval to `4`
* we made the arbitrary choice that seperaors should be **left** associative.  So "1;2;4" should parse to `((1;2);4)`.  (Mark: sorry for the typeo)
* Some hint files have been added [Lang3Hint](src/lang/Lang3Hint.hs), [Lang4Hint](src/lang/Lang4Hint.hs), [Lang1ParserHint](src/parser/Lang1ParserHint.hs), [ReaderTest](src/ReaderTest.hs)  [StateTest](src/StateTest.hs)


There were some small typos:
in Lang4.hs
```
showPretty (Let s val inThis) d = parenthesize d 5  "let " ++ s ++ " = " ++ showPretty val 4 ++ " in " ++ showPretty inThis 5 -- binds most weakly
```
should be
```
showPretty (Let s val inThis) d = parenthesize d 5  ("let " ++ s ++ " = " ++ showPretty val 4 ++ " in " ++ showPretty inThis 5 ) -- binds most weakly
```
in Lang2.hs
```
showFullyParen (Print b)         = "print(" ++ show b ++ ")"
```
should be
```
showFullyParen (Print b)         = "print(" ++ showFullyParen b ++ ")"
```

in Lang3.hs

```
showFullyParen (Assign v b)      = "(" ++ v ++ " := " ++ show b ++ ")"
```
should be
```
showFullyParen (Assign v b)      = "(" ++ v ++ " := " ++ showFullyParen b ++ ")"
```


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
