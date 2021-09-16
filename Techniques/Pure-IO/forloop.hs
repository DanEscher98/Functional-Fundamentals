module Main (main) where
import           Control.Monad    (when)
import           Data.Foldable    (for_)
import           Data.Traversable (for)

main :: IO ()
main = do
    putStr "Numbers: "
    for_ [1..5] $ \i -> do
        when (odd i) $ do
            putStr $ show i ++ " "
    putStr "\n"

    putStr "Result: "
    ans <- for [1..10] $ \i -> do
    --  when (even i) $ do
        putStr $ show i ++ " "
        return (i ^ 2)
    putStr ("(sum: " ++ show (sum ans) ++ ")\n")
