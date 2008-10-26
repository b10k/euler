module Main where

import System.Environment
import Euler
import ONeillPrimes 

--totientChain n = n : (takeWhile (> 0) $ iterate eulerTotient (n-1))
totientChain n = n : (takeUpto (1==) $ iterate eulerTotient (n-1))

is25 = (== 25) . (length . totientChain)

--answer = sum $ filter is25 (takeWhile (<40000000) primes)

main = do
  [n] <- map read `fmap` getArgs
  print $ sum $ filter is25 (takeWhile (< n) primes)
