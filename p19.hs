module P19 where

import Euler

{-
 - 1 Jan 1900 = Monday
 -
 - Thirty days has September,
 - April, June and November.
 - All the rest have thirty-one,
 - Saving February alone,
 - Which has twenty-eight, rain or shine.
 - And on leap years, twenty-nine.
 -
 - A leap year occurs on any year evenly divisible by 4,
 - but not on a century unless it is divisible by 400.
 -
 - How many Sundays fell on the first of the month during
 - the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 -
 - 1 jan 1901 = Tuesday
 -}

isLeap n | 400 `divides` n = True
         | 100 `divides` n = False
         | 4 `divides` n   = True
         | otherwise       = False

--Jan Feb March *April May *June July August *September October *November December
daysPerMonth     = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leapDaysPerMonth = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

makeYear n | isLeap n  = leapDaysPerMonth
           | otherwise = daysPerMonth

firstOfMonth y = scanl1 (+) years

years = 1 : (init $ concat $ map makeYear [1901 .. 2000])

sundays = [6,13..]
 
answer = length $ intersectSorted (firstOfMonth years) sundays
