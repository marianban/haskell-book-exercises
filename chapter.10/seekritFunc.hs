module SeekritFunc where
  seekritFunc x =
    (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))
