module Main where

import qualified Data.Map            as Map

import           Control.Monad.State
import           Evaluator           (SymTab, evaluate)
import           Lexer               (tokenize)
import           Parser              (parse)
import           System.Environment


constants = Map.fromList
    [ ("pi", pi)
    , ("e", exp 1.0)
    , ("phi", (1 + sqrt 5) / 2)
    ]

evalLoop :: String -> State SymTab String
evalLoop expression = do
    symtab <- get
    let toks = tokenize expression
    let tree = parse toks
    let (val, symtab') = evaluate tree symtab
    put symtab'
    return (expression ++ " => " ++ show val)

interpreter :: FilePath -> IO ()
interpreter file = do
    contents <- readFile file
    let fileLines = filter (not . null) . lines $ contents
     in do putStrLn
           . unlines
           . evalState (mapM evalLoop fileLines)
           $ constants

main :: IO ()
main = do
    args <- getArgs
    let fileName = args !! 1
     in interpreter "expressions.txt"
