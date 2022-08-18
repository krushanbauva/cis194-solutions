module Hw03.Golf (skips, localMaxima, histogram) where

takeNth :: Int -> [a] -> [a]
takeNth n list = [list !! i | i <- [n-1, n-1+n .. length list - 1]]

skips :: [a] -> [[a]]
skips list = [takeNth i list | i <- [1 .. length list]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) = if y > x && y > z then y:localMaxima(z:zs) else localMaxima(y:z:zs)
localMaxima _ = []

count :: [Integer] -> [Int]
count xs = map (\n -> length (filter (== n) xs)) [0..9]

nlines :: [Int] -> [String]
nlines [0,0,0,0,0,0,0,0,0,0] = []
nlines list = nlines newList ++ [str]
              where str = [if i>0 then '*' else ' ' | i <- list] ++ "\n"
                    newList = map (\x -> if x>0 then x-1 else 0) list

histogram :: [Integer] -> String
histogram list = concat $ nlines (count list) ++ ["==========\n"] ++ ["0123456789\n"]