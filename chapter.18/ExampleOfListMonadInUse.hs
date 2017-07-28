module ExampleOfListMonadInUse where
  twiceWhenEven :: [Integer] -> [Integer]
  twiceWhenEven xs = do
    x <- xs
    if even x
      then [x * x, x * x]
      else []
