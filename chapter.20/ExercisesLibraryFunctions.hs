import Data.Monoid hiding (fold)
import Prelude hiding (null, length)

-- 1. This and the next one are nicer with foldMap, but foldr is fine too.
sum :: (Foldable t, Num a) => t a -> a
sum xs = getSum $ foldMap Sum xs

-- 2. product :: (Foldable t, Num a) => t a -> a
product :: (Foldable t, Num a) => t a -> a
product xs = getProduct $ foldMap Product xs

-- 3. elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = foldr (\x' z -> z || (x' == x)) False xs

-- 4. minimum :: (Foldable t, Ord a) => t a -> Maybe a
newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  (Min Nothing) `mappend` x = x
  x `mappend` (Min Nothing) = x
  (Min (Just a)) `mappend` (Min (Just a')) = Min $ Just (min a a')

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = getMin $ foldMap (Min . Just) xs

-- 5. maximum :: (Foldable t, Ord a) =>
newtype Max a = Max { getMax :: Maybe a }

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  (Max Nothing) `mappend` x = x
  x `mappend` (Max Nothing) = x
  (Max (Just a)) `mappend` (Max (Just a')) = Max $ Just (max a a')

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = getMax $ foldMap (Max . Just) xs

-- 6. null :: (Foldable t) => t a -> Bool
null :: (Foldable t) => t a -> Bool
null xs = (foldr (\_ acc -> acc + 1) 0 xs) == 0

-- 7. length :: (Foldable t) => t a -> Int
length :: (Foldable t) => t a -> Int
length = foldr (\_ acc -> acc + 1) 0

-- 8. Some say this is all Foldable amounts to.
toList :: (Foldable t) => t a -> [a]
toList xs = foldMap (\x -> [x]) xs

-- 9. Hint: use foldMap.
-- | Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10. Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x acc -> mappend (f x) acc) mempty xs
