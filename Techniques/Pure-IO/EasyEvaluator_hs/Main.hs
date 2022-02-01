module Main where

import qualified Data.Map  as Map

import           Evaluator (evaluate)
import           Lexer     (tokenize)
import           Parser    (parse)

main :: IO ()
main = do
    loop (Map.fromList [("pi", pi), ("e", exp 1.0)])

loop symtab = do
    str <- getLine
    if null str
       then return ()
       else let toks = tokenize str
                tree = parse toks
                (val, symtab') = evaluate tree symtab
             in do
                print val
                loop symtab'
