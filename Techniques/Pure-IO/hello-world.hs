module Main (main) where
import           Data.Char
import           System.Directory
import           System.IO
import           System.IO.Unsafe

main :: IO ()
main = do
    s <- prompt "Set your name: "
    putStrLn ("Hello " ++ s)
    ifile <- getValidFile "Input file: "
    ofile <- getValidFile "Output file: "
    s <- readFile ifile
    writeFile ofile (map toLower (filter (not . isNumber) s))
    putStrLn "Goodbye!"


getValidFile :: String -> IO String
getValidFile text
    | unsafePerformIO . doesFileExist $ s = return s
    | otherwise = getValidFile "Try again: "
        where s = unsafePerformIO . prompt $ text

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout -- Buffering cuestion
    getLine

tomaLinea :: IO String
tomaLinea = getChar >>=
    (\c -> if c=='\n'
            then return ""
            else tomaLinea >>=
                (\s -> return (c:s)))

ponLinea :: String -> IO ()
ponLinea = foldr ((>>) . putChar) (putChar '\n')
