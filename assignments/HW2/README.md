# HW2
Due 6/6
## Instructions
* Fill in the bodies of the undefined functions and data
* DO NOT CHANGE THE TYPE SIGNATURES!

## Notes
* Sever bugs in the test code have been discovered and fixed, run `git pull upstream master` to get the fixes.  We hope the tests  are fixed, but if you find what looks like a test bug please let us know ASAP.
* For the `TypeProblems` you may give any answer that is consistent with the type specified.  You do not need to give a term that has the most general given type.  For instace: if asked for a term of type `Bool -> Bool`, both `\ b -> not b` and `\ x -> x` will get full credit.
* There are hint videos about the Lang problems: http://www.cs.bu.edu/fac/snyder/cs320/Homeworks%20and%20Labs/RPReplay_Final1559443437.MP4 , https://www.youtube.com/channel/UCfSqNB0yh99yuG4p4nzjPOA
* Map implementations should take advantage of the BST to be reasonably efficient.  We may deductuct points for obvous inefficiencies.    However, you do not need to do anything fancy like rebalencing your tree.

### Submit (similar to [week1](../week1))
1. run the tests by running ```cabal new-test``` 
1. run ```git status``` to make sure git is ok
1. run ```git commit -a -m "haskell is fun"``` to make a commit to your laptop
1. run ```git pull upstream master``` to get the latest tests
1. run the tests by running ```cabal new-test``` 
1. run ```git push``` to submit your commit to your private gitHub account
1. check that you can see your solutions on the website for your private repo

