module Main where

import Data.Char

a = concat $ map show [1..]
b = map (a !!) [0,9,99,999,9999,99999,999999]
readChar x = ord x - 48
answer = product $ map readChar b
