module Main where

import Hw01
import Hw03.Golf

main :: IO ()
main = do 
    print $ hanoi 4 "a" "b" "c"
    print $ validate 4012888888881881
    print $ skips "hello!"
    print $ localMaxima [2,9,5,6,1]
    putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
