module Main where

import           Control.Monad (when)
import           Data.Char     (toUpper)

writeFunk :: FilePath -> FilePath -> IO ()
writeFunk ifile ofile = do
    contents <- readFile ifile
    let fileLines = lines contents
    let newContents = map toUpper $ unlines . take 2 $ fileLines
    when (length newContents > 0) $
        writeFile ofile newContents


main :: IO ()
main = do
    writeFunk "ifile.txt" "ofile.txt"

-- loop :: SymTab -> IO ()
-- loop symtab = do
--     str <- getLine
--     if null str
--        then return ()
--        else let toks = tokenize str
--                 tree = parse toks
--                 (val, symtab') = evaluate tree symtab
--              in do
--                 print val
--                 loop symtab'
