import Test.QuickCheck
import Test.Hspec
import Lib

ceasarTestCase :: Positive Int -> Bool
ceasarTestCase (Positive x) = "test" == (cipher (-x) (cipher x "test"))

vigenereTestCase :: [Char] -> Bool
vigenereTestCase xs = xs == (vigenereDecode "keyword" (vigenere "keyword" xs))

main :: IO ()
main = hspec $ do
  describe "Ceasar cipher" $ do
    it "works for arbitrary positive number" $ do
      property ceasarTestCase
  describe "Vigenere cipher" $ do
    it "works for arbitrary string key" $ do
      property vigenereTestCase
