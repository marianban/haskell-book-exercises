module LibraryForEither where
  {-
  Write each of the following functions. If more than one possible
  unique function exists for the type, use common sense to determine
  what it should do.
  -}
  {-
  1. Try to eventually arrive at a solution that uses foldr, even if
  earlier versions don’t use foldr.
  -}
  lefts' :: [Either a b] -> [a]
  lefts' xs = foldr f [] xs where
    f :: Either a b -> [a] -> [a]
    f (Left x) acc = x : acc
    f (Right _) acc = acc

  {-
  2. Same as the last one. Use foldr eventually.
  -}
  rights' :: [Either a b] -> [b]
  rights' xs = foldr f [] xs where
    f :: Either a b -> [b] -> [b]
    f (Left _) acc = acc
    f (Right x) acc = x : acc

  {-
  3. partitionEithers' :: [Either a b] -> ([a], [b])
  -}
  partitionEithers' xs = (lefts' xs, rights' xs)

  -- 4. eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe' _ (Left _) = Nothing
  eitherMaybe' f (Right x) = Just $ f x

  -- 5. This is a general catamorphism for Either values.
  either' :: (a -> c) -> (b -> c) -> Either a b -> c
  either' f _ (Left x) = f x
  either' _ g (Right x) = g x

  -- 6. Same as before, but use the either' function you just wrote.
  eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe'' f x = either' g f' x where
    g _ = Nothing
    f' x = Just $ f x
