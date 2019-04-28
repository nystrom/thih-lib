module Main where

import System.Environment
import Run

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-e", arg] -> tiString arg
        args -> mapM_ tiFile args
