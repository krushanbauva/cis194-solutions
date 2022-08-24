module Hw04 (fun1', fun2', xor, map', myFoldl, sieveSundaram) where

import Data.List((\\))

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = product $ map (subtract 2) (filter even xs)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

-- fun2' :: Integer -> Integer
-- fun2' n = iterate (\x -> if even x then x + fun2 (x `div` 2) else fun2 (3 * x + 1)) n !! 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1) 

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x+1 | x <- [1..n] \\ removeList]
                  where removeList = [i+j+2*i*j | i <- [1 .. n `div` 2], j <- [1 .. n `div` 2], i <= j, i+j+2*i*j <= n]