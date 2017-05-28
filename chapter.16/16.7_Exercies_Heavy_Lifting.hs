module ExercisesHeavyLifting where
  -- 1
  a = fmap (+1) $ read "[1]" :: [Int]

  -- 2
  b = (fmap . fmap) (++ "lol") (Just ["Hi", "Hello"])

  -- 3
  c = fmap (*2) (\x -> x - 2)

  -- 4
  d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

  -- 5

  ioi = readIO "1" :: IO Integer
  changed :: IO Integer
  changed = fmap read (fmap ("123"++) (fmap show ioi))
  e :: IO Integer
  e = fmap (*3) changed
