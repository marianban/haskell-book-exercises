module MyFilter where
  myFilter :: (a -> Bool) -> [a] -> [a]
  myFilter f = foldr doFilter [] where
    doFilter x acc
      | f x = x : acc
      | otherwise = acc
  -- myFilter f = foldr (\x acc -> if (f x) then x : acc else acc) []
