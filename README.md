# CIS194: Winter 2016

Click [here](http://coursework-progress.herokuapp.com/) to chart your progress through the course.

## Schedule

Week | Lecture                                                | Assignment              | Due     |
---- | ------------------------------------------------------ | ----------------------- | ------- |
1    | [Introduction to Haskell][1]                           | [cc numbers, hanoi][21] | Jan. 12 |
2    | [Algebraic Data Types][2]                              | [log parsing][22]       | Jan. 19 |
3    | [Recursion patterns, polymorphism, and the Prelude][3] | [code golf][23]         | Jan. 26 |
4    | [Parametric Polymorphism][4]                           | [BST, recursion][24]    | Feb. 2  |
5    | [More polymorphism and type classes][5]                | [calculator][25]        | Feb. 9  |
6    | [Lazy evaluation][6]                                   | [streams][26]           | Feb. 16 |
7    | [Monoids, I/O][7]                                      | [markets, JSON][27]     | Feb. 23 |
8    | [Functors][8]                                          | n/a                     | n/a     |
9    | [Applicative functors (part 1)][9]                     | [parsers, part one][28] | Mar. 1  |
10   | [Applicative functors (part 2)][10]                    | [parsers, part two][29] | Mar. 8  |
11   | [Monads][11]                                           | [risk][30]              | Mar. 15 |
12   | [QuickCheck][12]                                       | [rings][31]             | Mar. 22 |

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
  [12]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/lectures/Week12L-quickcheck.pdf

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
  [31]: https://github.com/laser/cis-194-winter-2016/blob/master/pdfs/assignments/Week12A-quickcheck.pdf

## Getting Started

### Install Stack

This project uses [Stack](http://docs.haskellstack.org/en/stable/README.html) to build and test the assignments. To install it, [follow the steps here](http://docs.haskellstack.org/en/stable/README.html#how-to-install). 

### Installing Project Dependencies

Project dependencies (including GHC version) are specified in the project's `.cabal` file and are downloaded and built by Stack. To build the project along with all of its dependencies, run the `stack build` command from the root directory of the project.

### Running the Tests

Assignments are tested with Hspec and QuickCheck. The full test suite can be run by running the `stack test` command.

## Useful Links

* [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
* [Hoogle](https://www.haskell.org/hoogle/)
* [Standard library aka Prelude documentation](http://hackage.haskell.org/package/base)
