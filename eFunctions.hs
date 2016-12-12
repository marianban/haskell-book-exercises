module EFunctions where
  fifthChar :: [a] -> [a]
  fifthChar x = drop 5 x
  addExMark :: String -> String
  addExMark x = x ++ "!"
  rvrs :: String -> String
  rvrs x = (drop 6 x) ++ " " ++ (take 5 x)
