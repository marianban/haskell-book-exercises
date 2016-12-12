module CapitalizeAll where
  import Data.Char
  capitalizeAll :: String -> String
  capitalizeAll [] = []
  capitalizeAll (x : xs) = toUpper x : capitalizeAll xs
