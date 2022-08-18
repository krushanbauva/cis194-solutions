module Hw01 (hanoi, validate) where

-- Validating Credit Card Numbers
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n > 0     = n `mod` 10 : toDigitsRev (n `div` 10)
    | otherwise = []

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs)
    | even (length (x:y:xs))   = 2*x:y:doubleEveryOther xs
    | otherwise                = x:2*y:doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) 
    | x `div` 10 == 0   = x + sumDigits xs 
    | otherwise         = x `mod` 10 + x `div` 10 + sumDigits xs 

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0


-- The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 p3 = [(p1, p2)]
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ hanoi 1 p1 p2 p3 ++ hanoi (n-1) p3 p2 p1 