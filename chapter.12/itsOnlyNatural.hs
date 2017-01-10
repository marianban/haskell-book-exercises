module Natural where
  {-
  You’ll be presented with a datatype to represent the natural numbers.
  The only values representable with the naturals are whole numbers
  from zero to infinity. Your task will be to implement functions to
  convert Naturals to Integers and Integers to Naturals. The conversion
  from Naturals to Integers won’t return Maybe because Integer is a strict
  superset of Natural. Any Natural can be represented by an Integer,
  but the same is not true of any Integer. Negative numbers are not
  valid natural numbers.
  -}

  data Nat = Zero | Succ Nat deriving (Eq, Show)
  natToInteger :: Nat -> Integer
  natToInteger Zero = 0
  natToInteger (Succ x) = 1 + natToInteger x

  integerToNat :: Integer -> Maybe Nat
  integerToNat x
    | x < 0 = Nothing
    | otherwise = Just $ toNat x
    where
      toNat :: Integer -> Nat
      toNat 0 = Zero
      toNat x = Succ $ toNat (x - 1)

  unbox (Just x) = x

  main :: IO()
  main = print (natToInteger (unbox (integerToNat 5)))
