module Main where

import Hw01

main :: IO ()
main = do 
    print $ hanoi 4 "a" "b" "c"
    print $ validate 4012888888881881

