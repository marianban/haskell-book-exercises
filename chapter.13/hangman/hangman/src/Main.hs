module Main where

import Lib
import Data.Char (toLower)

main :: IO ()
main = do
  -- word <- randomWord'
  let word = "test"
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
