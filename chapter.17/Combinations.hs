module Combinations where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
-- it's same as:
-- combos xs ys zs = (,,) <$> xs <*> ys <*> zs
main :: IO ()
main = print $ combos stops vowels stops
