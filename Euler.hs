{-# LANGUAGE NPlusKPatterns #-}
module Euler where

import Data.Bits (shiftR)
--import Prelude hiding (map, (++), filter, head, last, tail, init, null, length, (!!), reverse, foldl, foldl1, foldr, foldr1, and, or, any, all, sum, product, concat, concatMap, maximum, minimum, scanl, scanl1, scanr, scanr1, iterate, repeat, replicate, cycle, take, drop, splitAt, takeWhile, dropWhile, span, break, elem, notElem, lookup, zip, zip3, zipWith, zipWith3, unzip, unzip3, lines, words, unlines, unwords)
--import Data.List.Stream
import Data.List
import ONeillPrimes (primes)
import RabinMiller

fact n = product [1..n]

divides d n = rem n d == 0

-- Prime factorization

{-# SPECIALIZE factor :: Int -> [Int] #-}
{-# SPECIALIZE factor :: Integer -> [Integer] #-}
factor n = f' n primes
  where
    f' n (p:ps) | p*p > n        = [n]
                | n `mod` p == 0 = p : (f' (div n p) (p:ps))
                | otherwise      = f' n ps

{-# SPECIALIZE prime :: Int -> Bool #-}
{-# SPECIALIZE prime :: Integer -> Bool #-}
prime n | n <= 1     = False
        | otherwise  = head (factor n) == n

isPrime n | n < 10^9  = prime n
          | odd n     = rabin n
          | otherwise = False

-- Gives unique divisors of a number n, including 1 and n
divisors n = 1 : (sort $ nub $ map product $ sublists (factor n))

-- Big prime factorization
-- This is still really slow for some reason
prime1 plist n | n < 2^24   = n `elem` plist
               | n > (2^48) = prime n
               | otherwise  = ld' plist n == n
  where
    ld' (k:ks) n | k `divides` n = k
                 | k*k > n       = n
                 | otherwise     = ld' ks n

-- Euler totient function
-- eulerTotient n = sum $ filter (1==) (map (gcd n) [1..(n-1)])

{-# SPECIALIZE eulerTotient :: Int -> Int #-}
{-# SPECIALIZE eulerTotient :: Integer -> Integer #-}
eulerTotient n = 
  product $ map (\(p,k) -> (p-1) * (p^(k-1))) $ f (factor n)
    where
      f xx = f' xx []
      f' [] yy = yy
      f' (x:xs) []                          = f' xs [(x,1)]
      f' (x:xs) yy@(y@(p,s):ys) | x == p    = f' xs ((p,s+1):ys)
                                | otherwise = f' xs ((x,1):yy)


-- Exponentiation modulo m  -- (b^e mod m)
expmod b e m = f b e m 1
    where f b e m z | e == 0 = z
                    | even e = f (b * b `mod` m) (e `shiftR` 1) m z
                    | otherwise = f (b * b `mod` m) (e `shiftR` 1) m (z * b `mod` m)

-- Hyperexponentiation mod m -- depends on expmod
hypermod a 1 m = mod a m
hypermod a (n+1) m = expmod a (hypermod a n m) m


-- List utilties

-- The intersection of two sorted lists. Can handle any combination of infinite
-- and finite lists.
intersectSorted as bs = inter as bs
  where
    inter [] _ = []
    inter _ [] = []
    inter xs@(x:xt) ys@(y:yt) = 
      case compare x y of
        LT -> inter xt ys
        EQ -> x : (inter xt yt)
        GT -> inter xs yt

split :: (Eq a) => [a] -> a -> [[a]]
split s d = case dropWhile (d ==) s of
                [] -> []
                s' -> w : split s'' d
                      where (w, s'') = break (d ==) s'

-- sublists: Generate all sublists of a list
sublists [] = []
sublists (x:xs) = [x] : ((map (x:) (sublists xs)) ++ (sublists xs))

-- takeUpto: Return everything up to and including the first element for which p is true
takeUpto _ []          =  []
takeUpto p (x:xs) 
           | p x       = [x]
           | otherwise = x : takeUpto p xs

-- n `choose` r = n! / (r!(n-r)!)
choose n r = (fact n) `div` ((fact r) * (fact (n-r)))

{-# SPECIALIZE digits :: Int -> [Int] #-}
{-# SPECIALIZE digits :: Integer -> [Integer] #-}
digits n = itod n []
itod x xs
  | x < 10 = x : xs
  | otherwise = case (x `rem` 10) of
                  y -> itod (x `quot` 10) (y : xs)

undigits a = un 0 a
  where
    un m [] = m
    un m (n:ns) = un ((m * 10) + n) ns

listpandigitals = permutations [0,1,2,3,4,5,6,7,8,9]

pandigitals n = map undigits $ permutations [1..n]

