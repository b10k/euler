module P34 where

import Euler (fact, digits)

curious n = (sum $ map fact (digits n)) == n
