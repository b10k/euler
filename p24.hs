module P24 where

import Euler
import Data.List (delete)

perms [] _ = []
perms xs n = x : perms (delete x xs) (mod n m)
  where m = fact $ length xs - 1
        y = div n m
        x = xs!!y

