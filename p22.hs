module Main where

import System.Environment (getArgs)
import Euler (split)
import Data.List (delete, sort)
import Data.Char (toUpper, ord, chr)

cleanup a = delete '"' $ delete '"' a

ordStr s = sum $ map ordBaseA s

ordBaseA c = 1 + (ord (toUpper c) - ord 'A')

main = do
--    [file] <- getArgs
    f <- readFile "names.txt"
    let names = sort $ map cleanup $ split f ','
    let scores = map ordStr names
    let score = sum $ zipWith (*) scores [1..]
    return score
