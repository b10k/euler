module PrimeSieve (primes) where

--import Prelude hiding (map, (++), filter, head, last, tail, init, null, length, (!!), reverse, foldl, foldl1, foldr, foldr1, and, or, any, all, sum, product, concat, concatMap, maximum, minimum, scanl, scanl1, scanr, scanr1, iterate, repeat, replicate, cycle, take, drop, splitAt, takeWhile, dropWhile, span, break, elem, notElem, lookup, zip, zip3, zipWith, zipWith3, unzip, unzip3)
--import Data.List.Stream

data (Integral a) => Wheel a = Wheel a [a] deriving Show

{-# SPECIALIZE wheels :: [Wheel Int] #-}
{-# SPECIALIZE wheels :: [Wheel Integer] #-}
wheels :: (Integral a) => [Wheel a]
wheels = 
  Wheel 1 [1] :
  zipWith nextSize wheels primes

{-# SPECIALIZE nextSize :: Wheel Int -> Int -> Wheel Int #-}
{-# SPECIALIZE nextSize :: Wheel Integer -> Integer -> Wheel Integer #-}
nextSize :: (Integral a) => Wheel a -> a -> Wheel a
nextSize (Wheel s ns) p =
  Wheel (s*p) [n' | o <- [0, s..(p-1)*s],
                    n <- ns,
                    n' <- [n+o], n' `mod` p > 0]

{-# SPECIALIZE sieve :: [Wheel Int] -> [Int] -> [Int] -> [Int] #-}
{-# SPECIALIZE sieve :: [Wheel Integer] -> [Integer] -> [Integer] -> [Integer] #-}
sieve :: (Integral a) => [Wheel a] -> [a] -> [a] -> [a]
sieve ((Wheel s ns) : ws) ps qs =
  {-# SCC "Woot" #-} [n' | o <- {-# SCC "one" #-} s : [2*s, 3*s..(head ps-1)*s],
                           n <- {-# SCC "two" #-} ns,
                           n' <- [n+o], s <= 2 || {-# SCC "three" #-} noFactorIn ps qs n' ]
  ++ sieve ws (tail ps) (tail qs)
{- The problem here is noFactorIn is doing trial division.  There shouldn't be any of that -}

{-# SPECIALIZE noFactorIn :: [Int] -> [Int] -> Int -> Bool #-}
{-# SPECIALIZE noFactorIn :: [Integer] -> [Integer] -> Integer -> Bool #-}
noFactorIn :: (Integral a) => [a] -> [a] -> a -> Bool
noFactorIn (p:ps) (q:qs) x = {-# SCC "noFactorIn" #-} q > x || x `mod` p > 0 && noFactorIn ps qs x

{-# SPECIALIZE squares :: [Int] #-}
{-# SPECIALIZE squares :: [Integer] #-}
squares :: (Integral a) => [a]
squares = [p*p | p <- primes]

{-# SPECIALIZE primes :: [Int] #-}
{-# SPECIALIZE primes :: [Integer] #-}
primes :: (Integral a) => [a]
primes = sieve wheels primes squares
