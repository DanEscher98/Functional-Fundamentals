module Main (main) where

main :: IO ()
main = do {
          print ([x | x <- [0..10], even x]);
          putStrLn "Hello World";
          putStrLn "Bye";
          return ()
          }

tomaLinea :: IO String
tomaLinea = getChar >>=
    (\c -> if c=='\n'
            then return ""
            else tomaLinea >>=
                (\s -> return (c:s)))

ponLinea :: String -> IO ()
ponLinea = foldr ((>>) . putChar) (putChar '\n')
