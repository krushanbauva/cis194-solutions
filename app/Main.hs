module Main where

import Hw01
import Hw03.Golf
import Hw04

main :: IO ()
main = do 
    print $ hanoi 4 "a" "b" "c"
    print $ validate 4012888888881881
    print $ skips "hello!"
    print $ localMaxima [2,9,5,6,1]
    putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
    print $ fun1' [3,8,9,15,12,16,19,24]
    print $ fun2' 123
    print $ xor [False, True, False, False, True, False, True]
    print $ map' (*3) [1..10]
    print $ myFoldl (-) 0 [1..5]
    print $ sieveSundaram 100
