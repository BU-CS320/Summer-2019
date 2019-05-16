# HW0

If you are a mac user you may need to follow the instructions under [mac-hints.md](mac-hints.md).

## Setup
* Install the latest version(8.6.3) of [Haskell Platform](https://www.haskell.org/platform/). 
  - **Notice For Linux User**: 
    If you are on Linux (especially Ubuntu), you might NOT get the latest version (`GHC 8.6.3` and `cabal 2.4.1.0`) by following the instruction on Haskell Platform.
    Although the older version of GHC probably will not be a problem for this course, 
    please note that your code will be tested on GHC version 8.6.3.
    Here are two of the options to get the latest version of GHC: [ghcup](https://github.com/haskell/ghcup) and [stack](https://docs.haskellstack.org/en/stable/README/).
  - Open your terminal and type ```ghci``` you should see ```GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help```
  - Type ```:q``` to quit
* If you have not already, sign up for a free GitHub account following these [instructions](https://help.github.com/articles/signing-up-for-a-new-github-account/)
* If you have not already, set up git on your computer following these [instructions](https://help.github.com/articles/set-up-git/)
* run ```cabal new-update```
* run ```cabal new-install tasty tasty-hunit --lib``` to get the test libraries

## Setup Your Local Repository
* Follow this [link](https://classroom.github.com/a/7OYEsnd0) to create your private repo for this class.
  * Press the "Accept this assignment" button, this will create a private repository for your solutions
  * When the cloning finishes, you will see a link like "Your assignment has been created here: [link to your new private repo]", click that link.  You will know you are looking at your private repo becuase you will see ![Lock icon BU-CS320/cs320-hw- your user name Private](img/private-repo.png) on the top of the web page (where your username will be posted instead of "marklemay").
* Clone your personal assignment repo by following the instructions [here](https://help.github.com/articles/cloning-a-repository/)
* ```cd``` into the newly created directory by typing ```cd cs320-hw-username``` with ```username``` replaced with your user name
* You always want to take advantage of the latest corrections to the assignments and shared tests so we will add the main repository as a source
  * check the status of your repo: ```git status```
  * In your terminal type ```git remote add upstream https://github.com/BU-CS320/Spring-2019.git```
  * check the status of your repo: ```git status```
  * check that it worked by typing ```git remote -v```.  You should see see the line ```upstream https://github.com/BU-CS320/Spring-2019.git (fetch)```
  * You always want to keep your assignment up to date by running ```git pull upstream master```, do that now
  * check the status of your repo: ```git status```
  
## First Haskell Program
* ```cd```  into this assignment (```cd assignments/week1```), first we will write a greeting
  * run ```cabal new-repl``` (or ```cabal repl``` if gets "Failed to load interface") in the terminal (it is important that you do this in the ```week1``` directory).  You should see 
```
Preprocessing library for week1-0.1.0.0..
GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Greeting         ( src\Greeting.hs, interpreted )
Ok, one module loaded.
```
* let's try to run a greeting by typing ```greeting```.  You should see an error
  * fix the error by opening ```week1/src/Greeting.hs``` in your favorite text editor on the line that says ```greeting = undefined``` change ```undefined``` to your favourite greeting in quotes.
  * save the file
  * reset the terminal by typing ```:reload```
  * now if you type ```greeting``` into the terminal you should see your greeting
* Haskell is a functional language so define the identity function by changing ```ident x = undefined``` to  ```ident x = x```
  * reset the terminal by typing ```:reload```
  * Haskell doesn't let us see the definition of functions, but we can test by running ```ident greeting``` and see if we get back the greeting we expect
  * type ```:q``` to quit the REPL in the 
* run the tests by running ```cabal new-test```, some students may need to run ```cabal new-configure --enable-tests``` first
* check the status of your repo: ```git status```, you should see that the files have changed
* make a commit by typing ```git commit -a -m "my first commit"``` into the console
* check the status of your repo: ```git status```
* submit your work by typing ```git push```
* check the status of your repo: ```git status```
* check that you can see your solutions on the website for your private repo


## A Quick Survey
Complete the survey by adding your answers to [survey.md](survey.md).  You may do this through the with a editor and committing and pushing or through the GitHub interface.

## ```git``` issues
If you are having ```git``` issues run ```git status``` and post on piazza for help.
