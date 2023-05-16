# D7012E-Haskell-Lab3
--Christoffer Lindkvist
In this assignment you will create a parser and an interpreter for a small imperative language. The main goal is to get acquainted with a monadic way of solving a classical problem in computer science. 

Structure:

CoreParser.hs
defines the Parser type and implements the three elementary parsers, char, return and fail,
and the basic parser operators #, !, ?, #>, and >->, described in Lennart Andersson's
1 av 4
description.
The class Parse with signatures for parse, toString, and fromString with an implementation
for the last one is introduced.
The representation of the Parser type is visible outside the module, but this visibilty should not
be exploited.

Parser.hs
contains a number of derived parsers and parser operators.

Expr.hs
contains a data type for representing an arithmetic expression, an expression parser, an
expression evaluator, and a function for converting the representation to a string.

Dictionary.hs
contains a data type for representing a dictionary.

Statement.hs
contains a data type for representing a statement, a statement parser, a function to interpret a
list of statements, and a function for converting the representation to a string.

Program.hs
contains a data type for representing a program, a program parser, a program interpreter, and a
function for converting the representation to a string.

Test*.hs
contain test data.

In a test using the program in the introduction with the following definitions
 src = "read k; read n; m:=1; ... "
 p = Program.fromString src
the expression Program.exec p [3,16] should return [3,6,9,12,15].
