[![CircleCI](https://circleci.com/gh/dlomsak/regex-deriv.svg?style=shield)](https://circleci.com/gh/dlomsak/regex-deriv)

Regex-Deriv
==============
A Scala regular expression implementation with derivative-based evaluation described in the paper [Regular-expression derivatives re-examined](http://people.cs.uchicago.edu/~jhr/papers/2009/jfp-re-derivatives.pdf) by Scott Owens, John Reppy, and Aaron Turon.

Goals
-----
The main goal is to provide a regex library that avoids backtracking and extra-regular features (e.g., back matching) to achieve stable performance without pathological cases. While the Thompson caching NFA construction is a well-known way to achieve this, the state space can get large. Finally, the lexer/parser should be extensible to support additional syntactic elements that expand into standard regular expressions.

Roadmap
-------
* ~~Lexer and parser for basic regex operators~~
* ~~Implement regex derivatives for all AST structures and method for performing string matching~~
* ~~add tests and use property-based testing~~
* ~~CI with code coverage~~
* ~~support character classes~~
* ~~implement automaton construction, move away from matching via AST re-writing~~
* support common escape sequences such as \d for numbers
* ~~support bounded repetition (e.g., "r{n, m}" matches n to m occurrences of r)~~
* support group extraction
* support named groups ala [Regular Expression Sub-matching using Partial Derivatives](http://www.home.hs-karlsruhe.de/~suma0002/publications/ppdp12-part-deriv-sub-match.pdf) by Martin Sulzmann and Kenny Zhuo Ming Lu
* performance testing against scala.util.matching.regex
