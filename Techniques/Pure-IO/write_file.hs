module Main where

import           Control.Monad (when)
import           Data.Char     (toUpper)

writeFunk :: FilePath -> FilePath -> IO ()
writeFunk ifile ofile = do
    contents <- readFile ifile
    let newContents = map toUpper contents
    when (length newContents > 0) $
        writeFile ofile newContents

main :: IO ()
main = do
    writeFunk "ifile.txt" "ofile.txt"
