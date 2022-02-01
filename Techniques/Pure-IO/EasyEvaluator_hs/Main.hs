module Main where

import qualified Data.Map      as Map

import           Control.Monad (when)
import           Data.Char     (toUpper)
import           Evaluator     (SymTab, evaluate)
import           Lexer         (tokenize)
import           Parser        (parse)


constants = Map.fromList
    [ ("pi", pi)
    , ("e", exp 1.0)
    , ("phi", (1 + sqrt 5) / 2)
    ]

loop :: SymTab -> IO ()
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

main :: IO ()
main = do loop constants

writeFunk :: FilePath -> FilePath -> IO ()
writeFunk ifile ofile = do
    contents <- readFile ifile
    let fileLines = lines contents
    let newContents = map toUpper $ unlines . take 2 $ fileLines
    when (length newContents > 0) $
        writeFile ofile newContents
