module Sieves where

import Euler (prime)

primesT = filter prime [1..]

-- Sieve of Eratosthenes
primesE1 = sieve [2..]
	where sieve (p:ns) = p : sieve (filter (notdiv p) ns)
	      notdiv p n = n `mod` p /= 0

-- Circular programming sive of Eratosthenes
primesE2 = sieve [2..]
  where
    sieve (p:xs) = p : [x | x <- xs, noFactorIn primesE2 squares x]
    noFactorIn (p:ps) (q:qs) x = q > x || x `mod` p > 0 && noFactorIn ps qs x
    squares = [p*p | p <- primesE2]


-- Implicit heap version
--primesI, nonprimes :: [Integer]
primesI    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f . map g . tail $ primesI
  where 
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt

-- Wheel sieves

-- set primes to be the current fastest prime generator
primes = primesW1

--wheel n = product (take n primesI)

-- Mark I

data Wheel = Wheel Int [Int] deriving Show

wheels = 
  Wheel 1 [1] :
  zipWith nextSize wheels primesW1

nextSize (Wheel s ns) p =
  Wheel (s*p) [n' | o <- [0, s..(p-1)*s],
                    n <- ns,
                    n' <- [n+o], n' `mod` p > 0]


primesW1 = sieve wheels primesW1 squares
  where
    sieve ((Wheel s ns) : ws) ps qs =
      [n' | o <- s : [2*s, 3*s..(head ps-1)*s],
            n <- ns,
            n' <- [n+o], s <= 2 || noFactorIn ps qs n' ]
      ++ sieve ws (tail ps) (tail qs)
    noFactorIn (p:ps) (q:qs) x = q > x || x `mod` p > 0 && noFactorIn ps qs x
    squares = [p*p | p <- primesW1]

