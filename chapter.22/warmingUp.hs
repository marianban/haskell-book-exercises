module WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . reverse

fmapped :: [Char] -> [Char]
fmapped = fmap cap reverse

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> reverse

monadic :: [Char] -> ([Char], [Char])
monadic = do
  a <- cap
  b <- reverse
  return (a, b)

monadic' :: [Char] -> ([Char], [Char])
monadic' = cap >>= \xs -> reverse >>= \xs' -> return (xs, xs') 
