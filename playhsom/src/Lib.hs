module Lib
    ( someFunc,
    module RandomMusic,
    module SimpleMusic,
    module AlgorithmicMusic,
    module Basics,
    ) where

import qualified AlgorithmicMusic ()
import qualified Basics           ()
import qualified RandomMusic      ()
import qualified SimpleMusic      ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
