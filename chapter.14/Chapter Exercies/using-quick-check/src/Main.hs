module Main where

import Test.QuickCheck
import Test.Hspec
import Lib (half, halfIndentity)
import Data.List (sort)
import Data.Char

propertyFn :: Int -> Bool
propertyFn x = ((round . halfIndentity . fromIntegral) x) == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs where
  go _ status@(_, False) = status
  go y (Nothing, t) = (Just y, t)
  go y (Just x, t) = (Just y, x >= y)

genSortedList :: (Ord a, Arbitrary a) => Gen ([a])
genSortedList = do
  a <- arbitrary
  return $ sort a

genSortedNumericList :: Gen ([Int])
genSortedNumericList = genSortedList

prop_sort :: Property
prop_sort = forAll genSortedNumericList listOrdered

plusAssociative (x, y, z) = x + (y + z) == (x + y) + z
plusCommulative (x, y) = x + y == y + x

gen3Numbers :: (Num a, Arbitrary a, Eq a, Ord a) => Gen((a, a, a))
gen3Numbers = do
  a <- arbitrary
  a' <- arbitrary
  a'' <- arbitrary
  return (a, a', a'')

gen3Integers :: Gen((Integer, Integer, Integer))
gen3Integers = gen3Numbers

get2Numbers :: (Num a, Arbitrary a) => Gen((a, a))
get2Numbers = do
  a <- arbitrary
  a' <- arbitrary
  return (a, a')

genInt :: Gen Int
genInt = do
  a <- arbitrary
  return a

genList :: Gen([Int])
genList = do
  a <- arbitrary
  return a

genBoundedList :: Int -> Gen([Int])
genBoundedList x = vectorOf x genInt

gen2Integers :: Gen((Integer, Integer))
gen2Integers = get2Numbers

prop_associative :: Property
prop_associative = forAll gen3Integers plusAssociative

prop_cummulative :: Property
prop_cummulative = forAll gen2Integers plusCommulative

multiplicationAssociative (x, y, z) = x * (y * z) == (x * y) * z

prop_multiplicationAssociative :: Property
prop_multiplicationAssociative = forAll gen3Integers multiplicationAssociative

multiplicationCummulative (x, y) = x * y == y * x

prop_multiplicationCummulative :: Property
prop_multiplicationCummulative = forAll gen2Integers multiplicationCummulative

checkQuotRem :: Positive Integer -> Positive Integer -> Bool
checkQuotRem (Positive x) (Positive y) = (quot x y)*y + (rem x y) == (x)

powerCommutative :: Positive Integer -> Positive Integer -> Bool
powerCommutative (Positive x) (Positive y) = x ^ y == y ^ x

checkReverse :: [Int] -> Bool
checkReverse xs = (reverse . reverse) xs == id xs

prop_checkReverse :: Property
prop_checkReverse = forAll (genBoundedList 3) checkReverse

check_dolar :: Positive Integer -> Bool
check_dolar (Positive x) = ((2^) $ (2^) x) == (((2^) . (2^)) x)


--check_twoFunctions :: ([Int], [Int]) -> Bool
--check_twoFunctions (xs, ys) = (foldr (:) ys xs) == (xs ++ ys)

--prop_twoFunctions :: Property
--prop_twoFunctions = forAll genList check_twoFunctions

genConcreteList :: Int -> Gen([Int])
genConcreteList x = (vectorOf x genInt)

prop_Length :: Int -> Property
prop_Length x = forAll (vectorOf x genInt) checkLength

checkLength :: [Int] -> Bool
checkLength xs = length(take (length xs) xs) == length(xs)

checkReadShow :: Int -> Bool
checkReadShow x = (read (show x)) == x

square x = x * x
-- this does not hold because of precision lost in sqrt function
--squareIdentity = square . sqrt . fromIntegral
--checkSquare :: Int -> Bool
--checkSquare x = (squareIdentity x) == x

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : []) = [toUpper x]
capitalizeWord (x : xs) = (toUpper x) : (lowered xs) where
  lowered :: String -> String
  lowered [] = []
  lowered xs = foldr (\y acc -> (toLower y) : acc) [] xs

checkIdempotence1 :: String -> Bool
checkIdempotence1 x = (twice capitalizeWord x) == (fourTimes capitalizeWord x)

checkIdempotence2 :: String -> Bool
checkIdempotence2 x = (twice sort x) == (fourTimes sort x)

{- Equal probabilites for each -}
data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = oneof [return Fulse , return Frue]

foolGen2 :: Gen Fool
foolGen2 = frequency [(1, return Fulse), (3, return Frue)]

instance Arbitrary Fool where
  arbitrary = foolGen

main :: IO ()
main = hspec $ do
  {- 1 -}
  describe "half" $ do
    it "half identity of any x should be equal to x" $ do
      property propertyFn
  {- 2 -}
  describe "sort" $ do
    it "returns sorted numeric list" $ do
      prop_sort
  {- 3 -}
  describe "plus" $ do
    it "is associative" $ do
      prop_associative
    it "is cummulative" $ do
      prop_cummulative
  {- 4 -}
  describe "multiplication" $ do
    it "is associative" $ do
      prop_multiplicationAssociative
    it "is cummulative" $ do
      prop_multiplicationCummulative
  {- 5 -}
  describe "quote rem" $ do
    it "passes qoute rem check" $ do
      property checkQuotRem
  {- 6 -}
  {-
  power is not commutative or associative
  describe "power" $ do
    it "is not associative" $ do
      property powerCommutative
  -}
  {- 7 -}
  {-
  describe "reverse" $ do
    it "executed twice equals identity of the list" $ do
      prop_checkReverse
  -}
  {- 8 -}
  {-
  describe "$" $ do
    it "fulfil its definition" $ do
      property check_dolar
  -}
  {- 9
  describe "two functions" $ do
    it "are equal" $ do
      prop_twoFunctions
  -}
  {-10-}
  describe "length" $ do
    it "should be equal length (take n xs) == n" $ do
      prop_Length 5
  describe "read" $ do
    it "should invert show" $ do
      property checkReadShow
  {- Idempotence -}
  describe "idenpotence" $ do
    it "should hold for word capitalization" $ do
      property checkIdempotence1
    it "should hold for word sort" $ do
      property checkIdempotence2
  {- Equal probabilites for each -}
  {- Hangman tests in chapter.13/hangman/src/Spec.hs -}
