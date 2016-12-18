module MyFoldR where
  myFoldr :: (a -> b -> b) -> b -> [a] -> b
  myFoldr f z (x : xs) =
    case xs of
      [] -> z
      (x : xs) -> f x (myFoldr f z xs)
