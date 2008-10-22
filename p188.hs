{- Find the last 8 digits of 1777↑↑1855.
 - a↑↑1 = a,
 - a↑↑(k+1) = a(a↑↑k)
 -}

module Main where

import Data.Bits
import Text.Show
import Debug.Trace

expmod1 a b m = a ^ b `mod` m
expmod b e m = f b e m 1
    where f b e m z | e == 0 = z
                    | even e = f (b * b `mod` m) (e `shiftR` 1) m z
                    | otherwise = f (b * b `mod` m) (e `shiftR` 1) m (z * b `mod` m)

--hyper :: (Integral a, Floating a) => a -> a -> a
hyper a 1 d = mod a (10^d)
hyper a (n+1) d = expmod a (hyper a n d) (10^d)

hyper1 a 1 = a
hyper1 a (n+1) = a ^ (hyper1 a n)
