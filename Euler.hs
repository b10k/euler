module Euler (factors, divisors, expmod, hypermod, split) where

import Data.Bits (shiftR)
import Data.List (nub, sort)

divides d n = rem n d == 0

-- Prime factorization
ld n = ldf 2 n

ldf k n | k `divides` n = k
        | k^2 > n       = n
        | otherwise     = ldf (k+1) n

factors n | n < 1     = error "Argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p)
                        where p = ld n


sublists [] = []
sublists (x:xs) = [x] : ((map (x:) (sublists xs)) ++ (sublists xs))

-- Gives unique divisors of a number n, including 1 and n
divisors n = 1 : (sort $ nub $ map product $ sublists (factors n))

-- Exponentiation modulo m  -- (b^e mod m)
expmod b e m = f b e m 1
    where f b e m z | e == 0 = z
                    | even e = f (b * b `mod` m) (e `shiftR` 1) m z
                    | otherwise = f (b * b `mod` m) (e `shiftR` 1) m (z * b `mod` m)

-- Hyperexponentiation mod m -- depends on expmod
hypermod a 1 m = mod a m
hypermod a (n+1) m = expmod a (hypermod a n m) m


-- List utilties

split :: (Eq a) => [a] -> a -> [[a]]
split s d = case dropWhile (d ==) s of
                [] -> []
                s' -> w : split s'' d
                      where (w, s'') = break (d ==) s'

