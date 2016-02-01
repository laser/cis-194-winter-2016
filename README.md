# CIS194: Winter 2016

Click [here](http://coursework-progress.herokuapp.com/) to chart your progress through the course.

## Schedule

Week | Lecture                                                | Assignment Due          | Due     | Presenter                  | 
---- | ------------------------------------------------------ | ----------------------- | ------- | -------------------------- |
1    | [Introduction to Haskell][1]                           | [cc numbers, hanoi][21] | Jan. 14 |                            |
2    | [Algebraic Data Types][2]                              | [log parsing][22]       | Jan. 21 |                            |
3    | [Recursion patterns, polymorphism, and the Prelude][3] | [code golf][23]         | Jan. 28 |                            |
4    | [Parametric Polymorphism][4]                           | [BST, recursion][24]    | Feb. 4  | Sean Shubin (Type Classes) |
5    | [More polymorphism and type classes][5]                | [calculator][25]        | Feb. 11 | Mary Flagstad (Streams)    |
6    | [Lazy evaluation][6]                                   | [streams][26]           | Feb. 18 | Greg Wiley (Monoids)       |  
7    | [Monoids, I/O][7]                                      | [markets, JSON][27]     | Feb. 25 | Joe Vargas (Functor)       |
8    | [Functors][8] [Applicative functors (part 1)][9]                     | [parsers, part one][28] | Mar. 3  | Hal Arnold (Applicative Functors) |
9    | [Applicative functors (part 2)][10]                    | [parsers, part two][29] | Mar. 10 |                            |
10   | [Monads][11]                                           | [risk][30]              | Mar. 17 | ???                        |
11   | TBD                                                    | TBD                     | Mar. 24 | ???                        |
12   | TBD                                                    | TBD                     | Mar. 31 | ???                        |

  [1]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week01L-intro.pdf
  [2]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week02L-ADTs.pdf
  [3]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week03L-recursion-and-polymorphism.pdf
  [4]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week04L-parametric-polymorphism.pdf
  [5]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week05L-type-classes.pdf
  [6]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week06L-laziness.pdf
  [7]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week07L-monoids-and-IO.pdf
  [8]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week08L-functors.pdf
  [9]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week09L-applicative.pdf
  [10]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week10L-applicative-cont.pdf
  [11]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week11L-monads.pdf

  [21]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week01A-intro.pdf
  [22]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week02A-ADTs.pdf
  [23]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week03A-recursion-and-polymorphism.pdf
  [24]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week04A-parametric-polymorphism.pdf
  [25]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week05A-type-classes.pdf
  [26]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week06A-laziness.pdf
  [27]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week07A-monoids-and-IO.pdf
  [28]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week09A-applicative.pdf
  [29]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week10A-applicative-cont.pdf
  [30]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week11A-monads.pdf

## Getting Started

### Install Stack

This project uses [Stack](http://docs.haskellstack.org/en/stable/README.html) to build and test the assignments. To install it, [follow the steps here](http://docs.haskellstack.org/en/stable/README.html#how-to-install).

Or, use Homebrew:

```
brew install haskell-stack
```

### Clone the Repo (Don't Fork!!)

```
15:07 $ git clone git@github.com:laser/cis-194-winter-2016.git
Cloning into 'cis-194-winter-2016'...
remote: Counting objects: 350, done.
remote: Compressing objects: 100% (9/9), done.
remote: Total 350 (delta 13), reused 10 (delta 10), pack-reused 331
Receiving objects: 100% (350/350), 9.41 MiB | 2.93 MiB/s, done.
Resolving deltas: 100% (134/134), done.
Checking connectivity... done.
```

### Create a Branch

Create a new branch off of `master` whose name is the same as your GitHub username. Then, push the branch to the remote (GitHub) repository. If you don't have permission to push to this repository, let me know and I'll grant them.

```
15:07 $ cd cis-194-winter-2016/
✔ ~/dev/cis-194-winter-2016 [master|✔]

15:09 $ git checkout -b laser
Switched to a new branch 'laser'
✔ ~/dev/cis-194-winter-2016 [laser|✔]

15:09 $ git push origin laser
Total 0 (delta 0), reused 0 (delta 0)
To git@github.com:laser/cis-194-winter-2016.git
 * [new branch]      laser -> laser
✔ ~/dev/cis-194-winter-2016 [laser|✔]
```

### Initialize Stack

We use Stack to build our project. After cloning the repository, run `stack setup`.


### Running the Tests

Assignments are tested with Hspec and QuickCheck. The full test suite can be run by running the `stack test` command.

To run a single test:

```
stack build && stack runghc test/Homework/Week01Spec.hs
```

To rebuild and run the tests each time one of the project's files change:

```
~/eswenson-healey> stack test --file-watch
```

To rebuild the project and re-run a single test when a file changes, look into something like [entr](http://entrproject.org/).

### Charting Your Progress

The `coursework-progress` application ([link here][100]) provides a basic view
of who is participating in the course and how far they've progressed.

  [100]: http://coursework-progress.herokuapp.com

## Useful Links

* [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
* [Hoogle](https://www.haskell.org/hoogle/)
* [Standard library aka Prelude documentation](http://hackage.haskell.org/package/base)
