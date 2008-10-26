module Main where

main =
  do
    a <- newarray ((1,1),(30,30)) (1.0 :: Double)
    b <- newarray ((1,1),(30,30)) (1.0 :: Double)

updateArray a b = do
  map [1..30] 

