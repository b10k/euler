module PrimeSieve (primes) where

data Wheel = Wheel Int [Int] deriving Show

wheels = 
  Wheel 1 [1] :
  zipWith nextSize wheels primes

nextSize (Wheel s ns) p =
  Wheel (s*p) [n' | o <- [0, s..(p-1)*s],
                    n <- ns,
                    n' <- [n+o], n' `mod` p > 0]

primes = sieve wheels primes squares
  where
    sieve ((Wheel s ns) : ws) ps qs =
      [n' | o <- s : [2*s, 3*s..(head ps-1)*s],
            n <- ns,
            n' <- [n+o], s <= 2 || noFactorIn ps qs n' ]
      ++ sieve ws (tail ps) (tail qs)
    noFactorIn (p:ps) (q:qs) x = q > x || x `mod` p > 0 && noFactorIn ps qs x
    squares = [p*p | p <- primes]

