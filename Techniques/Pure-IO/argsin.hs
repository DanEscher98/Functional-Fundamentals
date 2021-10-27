module Main (main) where
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    print $ if head args == "hi"
       then "Nice to meet you!"
       else "Oh well..."
    let x = "Goodbye!" in
        print x
    print x
        where x = "ok"

