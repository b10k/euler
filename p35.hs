module P35 where

import Euler

circular p = all prime nums
  where
    ps = show p
    l = length ps
    nums = (map (\n -> (read . (take l) . (drop n)) (cycle ps)) [0..l-1])
