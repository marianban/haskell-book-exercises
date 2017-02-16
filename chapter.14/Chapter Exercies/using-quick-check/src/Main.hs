module Main where

import Test.QuickCheck
import Test.Hspec
import Lib (half, halfIndentity)
import Data.List (sort)

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

check_dolar :: Positive Integer -> Bool
check_dolar (Positive x) = (2^) $ (2^) x == ((2^) . (2^)) x

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
  describe "reverse" $ do
    it "executed twice equals identity of the list" $ do
      property checkReverse
  {- 8 -}
  describe "$" $ do
    it "fulfil its definition" $ do
      property check_dolar
