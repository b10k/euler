module Euler where

import Data.Bits (shiftR)
import Data.List (nub, sort)

fact n = product [1..n]

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

prime n | n < 1     = False
        | n == 1    = False
        | otherwise = ld n == n

-- Big prime factorization
-- This is still really slow for some reason
prime1 plist n | n < 2^24   = n `elem` plist
               | n > (2^48) = prime n
               | otherwise  = ld' plist n == n
  where
    ld' (k:ks) n | k `divides` n = k
                 | k^2 > n       = n
                 | otherwise     = ld' ks n

primeList = 
  do f <- readFile "primes.txt"
     let p = (map read $ words f) :: [Int]
     return (2 : 3 : 5 : p)

-- Euler totient function
-- eulerTotient n = sum $ filter (1==) (map (gcd n) [1..(n-1)])

eulerTotient n = 
  let f = map (\n -> (head n, length n)) (group $ factors n)
  in product $ map (\(p,k) -> (p-1) * (p^(k-1))) f


-- List manipulation
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

-- n `combine` r = n! / (r!(n-r)!)
combine n r = (fact n) `div` ((fact r) * (fact (n-r)))

digits n = reverse (d' n)
  where d' 0 = []
        d' n = let (x,y) = n `divMod` 10 in y : d' x

undigits a = un 0 a
  where
    un m [] = m
    un m (n:ns) = un ((m * 10) + n) ns
