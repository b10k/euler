module Main where

-- Take every nth one four times, incrementing n by two at that point

takeDiagonal (a:as) = a : (td' (tail as) 4 1)
  where
    td' [] _ _ = []
    td' a 0 k = td' (drop 2 a) 4 (k+2)
    td' (x:xs) n k = x : (td' (drop k xs) (n-1) k)

main = return $ sum $ takeDiagonal [1..(1001*1001)]
