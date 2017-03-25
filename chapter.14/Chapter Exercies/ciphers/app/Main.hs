module Main where

import Test.QuickCheck
import Lib

test :: [Char]
test = "&[];/,`%₉"



main :: IO ()
main = putStrLn (show (test == (vigenereDecode "keyword" (vigenere "keyword" test))))
