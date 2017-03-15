Regex-Deriv
==============
A regular expression implementation with derivative-based evaluation in Scala, inspired by the paper [Regular Expression Sub-matching using Partial Derivatives](http://www.home.hs-karlsruhe.de/~suma0002/publications/ppdp12-part-deriv-sub-match.pdf) by Martin Sulzmann and Kenny Zhuo Ming Lu.

Goals
-----
The main goal is to provide a regex library that avoids backtracking and extra-regular features (e.g., back matching) to achieve stable performance without pathological cases. While the Thompson caching NFA construction is a well-known way to achieve this with state machines, this project takes a pure approach: computing a new regex as each character of input is read. Structural sharing / sub-AST re-use is done as often as possible to minimize allocations. Finally, the lexer/parser should be extensible to support additional syntax/semantics to traditional regular expressions.

Roadmap
-------
* ~~Lexer and parser for basic regex operators~~ (done)
* ~~Implement regex derivatives for all AST structures and method for performing string matching~~ (done)
* add tests
* CI with code coverage
* support character classes
* support common escape sequences such as \d for numbers
* support bounded repetition (e.g., "r{n, m}" matches n to m occurrences of r)
* support group extraction
* support named groups
* performance testing against scala.util.matching.regex
