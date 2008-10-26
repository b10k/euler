module Main where

rev = (read . reverse . show)

lychrel n = l (n + rev n) 1
  where
    l _ 50 = True
    l n i | n == rev n = False
          | otherwise  = l (n + rev n) (i+1)

list = filter lychrel [1..9999]
