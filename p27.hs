module Main where

import Euler 

countPrimes (a,b) = length $ takeWhile prime (map (\n -> n*n + a*n + b) [0..])

functions = [(a,b) | a <- [-999..999], b <- [-999..999]]

counts = map countPrimes functions


main = do
  let m = maximum counts
  print m
  let l = length $ takeWhile (m >) counts
  print $ functions !! l
