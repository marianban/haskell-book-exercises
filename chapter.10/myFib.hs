module MyFib where
  -- explanation http://stackoverflow.com/questions/32576983/big-0-of-fibonacci-number-using-scanl-haskell
  fibs = 1 : scanl (+) 1 fibs
  fibsN x = fibs !! x
  first20fibs = take 20 fibs
  fibsLessThan100 = filter (<100) fibs
