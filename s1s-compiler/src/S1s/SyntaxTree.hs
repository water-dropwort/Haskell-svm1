------------------------------------------------------------------------------
-- S1s言語構文木
------------------------------------------------------------------------------
module S1s.SyntaxTree where

import Data.Int

-- <program> ::= main '{' <expression> '}'
data Prog   = Prog Expr deriving Show

-- <opeas> ::= + | -
data OpeAS  = Add
            | Sub deriving Show

-- <opemd> ::= * | /
data OpeMD  = Mul
            | Div deriving Show

-- <factor> ::= <number> | (<expression>)
data Factor = NFactor Int8
            | EFactor Expr deriving Show

-- <term> ::= <factor> {<opemd> <factor>}
data Term   = Term Factor [(OpeMD, Factor)] deriving Show

-- <expression> ::= <term> {<opeas> <term>}
data Expr   = Expr Term [(OpeAS, Term)] deriving Show
