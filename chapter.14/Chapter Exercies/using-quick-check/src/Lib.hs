module Lib where

half :: Float -> Float
half x = x / 2

halfIndentity :: Float -> Float
halfIndentity = (*2) . half
{-
genSortedList :: (Ord a, Arbitrary a) => Gen ([a])
genSortedList = do
  a <- arbitrary
  return $ sort a

genSortedNumericList :: Gen ([Int])
genSortedNumericList = genSortedList
-}
