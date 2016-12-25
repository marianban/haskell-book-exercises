module StopVowelStop where
  stops = "pbtdkg"
  vowels = "aeiou"
  stopVowelStop :: [(Char,Char,Char)]
  -- no way to go stopVowelStop = map (\x -> map (\x' -> map (\x'' -> (x, x', x'')) stops) vowels) stops
  stopVowelStop = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']
  nounVerbsNoun nouns verbs = [(n, v, n') | n <- nouns, v <- verbs, n' <- nouns]
