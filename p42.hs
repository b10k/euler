module Main where

import System.IO
import Data.Char
import Data.List
import Euler

triangles = 1 : (zipWith (+) [2..] triangles)

cleanup a = delete '"' $ delete '"' a

ordBaseA c = 1 + (ord (toUpper c) - ord 'A')
ordStr s = sum $ map ordBaseA s

triangleWord w = (ordStr w) `elem` (take 100 triangles)

main = do
  f <- readFile "42.txt"
  let ws = sort $ map cleanup $ split f ','
  print $ length $ filter triangleWord ws

