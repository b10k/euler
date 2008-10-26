module P36 where

import Numeric
import Data.Char

showBin n = showIntAtBase 2 toChr n ""
toChr n = chr (ord '0' + fromIntegral n)
palDec n = show n == (reverse . show) n
palBin n = showBin n == (reverse . showBin) n

palindromes = filter (\n -> palDec n && palBin n) [1..999999]
