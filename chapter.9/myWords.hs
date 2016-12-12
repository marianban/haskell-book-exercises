module MyWords where
  myWords :: [Char] -> [[Char]]
  myWords sentence = go sentence [] where
    go [] output = reverse output
    go sentence output = go (drop 1 $ dropWhile (/=' ') sentence) ((takeWhile (/=' ') sentence) : output)
