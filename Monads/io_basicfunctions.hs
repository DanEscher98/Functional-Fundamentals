module Main (main) where
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           System.Directory
import           System.IO
import           System.IO.Unsafe

main :: IO ()
main = do
    name <- prompt "Set your name: "
    let greeting = "Hello " ++ name ++ "!"
    putStrLn greeting
    ifile <- getValidFile "Input file: "
    ofile <- getValidFile "Output file: "
    s <- readFile ifile
    writeFile ofile (map toLower (filter (not . isNumber) s))
    putStrLn "Goodbye!"

getValidFile :: String -> IO String
getValidFile text = do
    file <- prompt text
    exists <- doesFileExist file
    if exists then return file
              else getValidFile "Try again: "

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout -- Buffering cuestion
    getLine

tomaLinea :: IO String
tomaLinea = do
    c <- getChar
    guard (c /= '\n')
    s <- tomaLinea
    return (c:s)

ponLinea :: String -> IO ()
ponLinea = foldr ((>>) . putChar) (putChar '\n')

sumFoldState :: [Int] -> State Int ()
sumFoldState = foldr ((>>) . (modify . (+))) (return ())

reverseListSt :: [a] -> State [a] ()
reverseListSt = foldr ((>>) . (modify . (:))) (return ())

lastElem :: [a] -> State a ()
lastElem = foldr ((>>) . put) (return ())
