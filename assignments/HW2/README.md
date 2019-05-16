# This assignment is a DRAFT!
that means some problems may change slightly and the tests are incomplete.  Do not edit unless you are comfortable with merge conflicts

* Better tests are needed for the Languages.
* map tests need to be rewritten without the generic tricks, explicit tests for equality.
* instructions need cleaning up



# HW3
Due 6/1
## Instructions
* Fill in the bodies of the undefined functions and data
* DO NOT CHANGE THE TYPE SIGNATURES!

## Notes

* If the tests are running forever, and you are sure there are no mistakes in your code.  Make sure all the old test processes are terminated.  We think there is a bug in one of our testing libraries that won't runn new tests while the old processes are open, see [@622](https://piazza.com/class/jr9fgrf7efv7j0?cid=622)



* For addAllList if the input is empty then you should return empty list, but we will not test the case that the input is empty.
* For the `TypeProblems` you may not "cheat" by causing an error either directly or indirectly.  For instance `head []` will recieve no credit since it crashes.
* For the `TypeProblems` you may give any answer that is consistent with the type specified.  You do not need to give a term that has the most general given type.  For instace: if asked for a term of type `Bool -> Bool`, both `\ b -> not b` and `\ x -> x` will get full credit.
#### Langs
* There is are hint videos availible: https://www.youtube.com/channel/UCfSqNB0yh99yuG4p4nzjPOA
* In Lang3 if you define one variable in terms of something undefined, there should be no effect (the state shouldn't change) and the result is undefined.  For instance `x := y` in the state `{x -> 3}` should result in `(Nothing,{x -> 3})`
* In any Lang problem, when there is any ambiguity in which order to evaluate:  evaluate left to right. For instance `print(2); print(5)` should have `2` before `5`.
* `-- hint use lookup` should have been `-- hint use Map.lookup and Map.insert`
* If it is possible to fail in a language, propagate the failure as soon as possible.  For example, when bad is not in the state `x := bad; 2+2` in lang 3 should return `(Nothing,{})`
#### Map
Implementations should take advantage of the BST to be reasonalby efficient.  For instance, you do not need to rebalence your tree.

- `==`: When defining Eq on Map, maps are equal when they have the exact same key value pairs. For instance, `insert 1 'a' (insert 2 'b' empty) == insert 2 'b' (insert 1 'a' empty)`
  - Note: the order matters in equality of list, for example `[1, 2]` is not equal to `[2, 1]`

### Submit (similar to [week1](../week1))
1. run the tests by running ```cabal new-test``` 
1. run ```git status``` to make sure git is ok
1. run ```git commit -a -m "haskell is fun"``` to make a commit to your laptop
1. run ```git pull upstream master``` to get the latest tests
1. run the tests by running ```cabal new-test``` 
1. run ```git push``` to submit your commit to your private gitHub account
1. check that you can see your solutions on the website for your private repo

