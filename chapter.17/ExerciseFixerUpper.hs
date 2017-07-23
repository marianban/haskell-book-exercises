{-
Given the function and values provided, use (<$>) from Functor,
(<*>) and pure from the Applicative typeclass to fill in missing bits of
the broken code to make it work.
-}

-- 1.
first = const <$> Just "Hello" <*> pure "World"

-- 2.
second = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
