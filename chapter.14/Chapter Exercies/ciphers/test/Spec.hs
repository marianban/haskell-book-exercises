import Test.QuickCheck
import Test.Hspec
import Lib

ceasarTestCase :: Positive Int -> Bool
ceasarTestCase (Positive x) = "test" == (cipher (-x) (cipher x "test"))

vigenereTestCase :: [Char] -> Bool
vigenereTestCase xs = xs == (vigenereDecode "keyword" (vigenere "keyword" xs))

genLetter :: Gen Char
genLetter = elements "abcdeafghjklmnoprsqutvxyz123456789"

genWords :: Gen [Char]
genWords = resize 20 (listOf genLetter)

vingenereProperty :: Property
vingenereProperty = forAll genWords vigenereTestCase

main :: IO ()
main = hspec $ do
  describe "Ceasar cipher" $ do
    it "works for arbitrary positive number" $ do
      property ceasarTestCase
  describe "Vigenere cipher" $ do
    it "works for arbitrary string key" $ do
      vingenereProperty
