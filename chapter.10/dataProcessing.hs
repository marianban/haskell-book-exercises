module DataProcessing where
  import Data.Time

  data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

  theDatabase :: [DatabaseItem]
  theDatabase = [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbNumber 9003,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

  filterDbDate :: [DatabaseItem] -> [UTCTime]
  filterDbDate = foldr f [] where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate x) xs = x : xs
    f _ xs = xs
  {- filter version
  filterDbDate xs = map (\(DbDate x) -> x) (filter isUtcTime xs) where
    isUtcTime :: DatabaseItem -> Bool
    isUtcTime (DbDate _) = True
    isUtcTime _ = False
  -}
  filterDbNumber :: [DatabaseItem] -> [Integer]
  filterDbNumber xs = foldl f [] xs where
    f :: [Integer] -> DatabaseItem -> [Integer]
    f xs (DbNumber x) = x : xs
    f xs _ = xs
  {-
  filterDbNumber xs = map (\(DbNumber x) -> x) (filter isNumber xs) where
    isNumber :: DatabaseItem -> Bool
    isNumber (DbNumber _) = True
    isNumber _ = False
  -}
  mostRecent :: [DatabaseItem] -> UTCTime
  mostRecent = maximum . filterDbDate

  sumDb :: [DatabaseItem] -> Integer
  sumDb = sum . filterDbNumber

  avgDb :: [DatabaseItem] -> Double
  avgDb xs = (fromIntegral . sumDb) xs / ((fromIntegral . length . filterDbNumber) xs)
