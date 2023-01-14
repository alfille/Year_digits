# Year_digits
Use the year's digits to make equations for the numbers between 1 and 100

# Summary
Use the year's digits (e.g. "2" "0" "2 "3") to make as many of the numbers as possible between 1 and 100. Preferable with the digits in order (2023).

Example:

`2+0+2-3 = 1`

`2+0*23 = 2`

etc...

# Project
Written 2023 Paul H Alfille
paul.alfille@gmail.com

MIT License

# Inspiration
This problem appears as a yearly proposal in [MIT's Technology Review](https://www.technologyreview.com/2004/03/01/40269/puzzle-corner-12/)

>Y2004. How many integers from 1 to 100 can you form using the digits 2, 0, 0, and 4 exactly once each and the operators +, -, x (multiplication), / (division), and exponentiation? We desire solutions containing the minimum number of operators; and among solutions having a given number of operators, those using the digits in the order 2, 0, 0, and 4 are preferred. Parentheses may be used for grouping; they do not count as operators. A leading minus sign does count as an operator.

# Project
## Format
Our approach will be an exhaustive search of the possible equations. Some versions with progressive optimizations will be added.

## Platform
Haskell, a pure functional language will be used. Since I'm new to the language, elegance will hopefully come in later versions.

## Steps
* Separate digits of year.
* Permute digits (optional for out-of-order)
* All combinations of digits separate or combined (e.g. [2023], [20,23], [2,0,,2,3]...)
* Group each combination in pairs. (e.g. (2,((0,2),)3)) or ((2,0),(2,3)) ...)
* Try each possible binary operator on the pairs (+ - / * ^)
* Each term can also be negated (i.e. 2, -2)
* Sort by criterial

