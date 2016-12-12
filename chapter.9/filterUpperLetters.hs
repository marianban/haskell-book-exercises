module FilterUpperLeters where
  import Data.Char
  filterUpperLeters :: String -> String
  filterUpperLeters = filter isUpper
