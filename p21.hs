module Main where

import Euler
import Data.List (delete)

properSum n = sum $ delete n $ divisors n

amicable = [ x | x <- [2..9999], x == (properSum . properSum) x , x /= properSum x]
