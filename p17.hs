module Main where


digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Out of range"

teenToWord 10 = "ten"
teenToWord 11 = "eleven"
teenToWord 12 = "twelve"
teenToWord 13 = "thirteen"
teenToWord 14 = "fourteen"
teenToWord 15 = "fifteen"
teenToWord 16 = "sixteen"
teenToWord 17 = "seventeen"
teenToWord 18 = "eighteen"
teenToWord 19 = "nineteen"
teenToWord _  = error "Out of range"

secondDigit 1 = "ten"
secondDigit 2 = "twenty"
secondDigit 3 = "thirty"
secondDigit 4 = "forty"
secondDigit 5 = "fifty"
secondDigit 6 = "sixty"
secondDigit 7 = "seventy"
secondDigit 8 = "eighty"
secondDigit 9 = "ninety"
secondDigit _ = error "Out of range"

numberToWord n | n < 0     = error "Negative input"
               | n < 10    = digitToWord n
               | n < 20    = teenToWord n
               | n < 100   = secondDigit (n `div` 10) ++ (if n `mod` 10 /= 0 then digitToWord (n `mod` 10) else [])
               | n < 1000  = digitToWord (n `div` 100) ++ "hundred" ++ (if n `mod` 100 /= 0 then "and" ++ numberToWord (n `mod` 100) else [])
               | n == 1000 = "onethousand"
               | otherwise = error "Requires n < 1000"

main = return $ length $ concat $ map numberToWord [1..1000]
