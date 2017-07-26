-- 1. -- Type
-- []
-- Methods
-- pure :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]

-- 2. -- Type
-- IO
-- Methods
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. -- Type
-- (,) a
-- Methods
-- pure :: Monoid b => a -> (b, a)
-- (<*>) :: Monoid b => (b, (a -> c) -> (b, a) -> (b, c)

-- 4. -- Type
-- (->) e
-- Methods
pure :: a -> (e -> a)
(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
