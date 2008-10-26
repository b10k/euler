module P52 where

import Euler (digits)
import Data.List (sort)

sd = (sort . digits)
isRight n =  sd n == sd (2*n)
          && sd n == sd (3*n)
          && sd n == sd (4*n)
          && sd n == sd (5*n)
          && sd n == sd (6*n)

answer = head $ filter isRight [1..]
