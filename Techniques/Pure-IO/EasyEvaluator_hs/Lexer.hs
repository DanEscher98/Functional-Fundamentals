module Lexer (
    Operator(..),
    Token(..),
    tokenize,
    lookAhead,
    accept
    ) where

import           Data.Char
data Operator = Add | Sub | Mul | Div
    deriving (Show, Eq)

data Token
    = TokOp Operator
    | TokAssign
    | TokLParen
    | TokRParen
    | TokVar String
    | TokNum Double
    | TokEnd
    deriving (Show, Eq)

lookAhead :: [Token] -> Token
lookAhead []     = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept []     = error "Nothing to accept"
accept (_:ts) = ts

-------------------
-- TOKENIZE -------

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | elem c "+-*/" = TokOp (operator c) : tokenize cs
    | c == '=' = TokAssign : tokenize cs
    | c == '(' = TokLParen : tokenize cs
    | c == ')' = TokRParen : tokenize cs
    | isDigit c = number (c:cs)
    | isAlpha c = identifier (c:cs)
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize: " ++ [c]

operator :: Char -> Operator
operator c = case c of
               '+' -> Add
               '-' -> Sub
               '*' -> Mul
               '/' -> Div

number :: String -> [Token]
number cs =
    let (digs, cs') = span isDigit cs
     in TokNum (read digs) : tokenize cs'

identifier :: String -> [Token]
identifier cs =
    let (name, cs') = span isAlphaNum cs
     in TokVar name : tokenize cs'
