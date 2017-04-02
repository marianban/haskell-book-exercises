module Main where
  import Test.QuickCheck
  import Test.Hspec
  import Lib

  firsttCheck :: Int -> Bool
  firsttCheck x = x == x

  main :: IO ()
  main = hspec $ do
    describe "hangman" $ do
      describe "freshPuzzle" $ do
        it "returns fresh puzzle for a given word" $ do
          (freshPuzzle "puzzle") `shouldBe` (freshPuzzle "puzzle")
      describe "fillInCharacter" $ do
        it "quicktest" $ do
          property firsttCheck
        it "should fill existing character" $ do
          show(fillInCharacter (freshPuzzle "puzzle") 'z' 0) `shouldBe` "_ _ z z _ _ Guessed so far: z Wrong guesses: 0"
      describe "handleGuess" $ do
        it "retuns same puzzle for already guessed character" $ do
          newPuzzle <- handleGuess (Puzzle "puzzle" [Nothing, Nothing, Just 'z', Just 'z', Nothing, Nothing] ['z'] 0) 'z'
          newPuzzle `shouldBe` (Puzzle "puzzle" [Nothing, Nothing, Just 'z', Just 'z', Nothing, Nothing] ['z'] 0)
        it "return puzzle with increased miss count for bad guess" $ do
          newPuzzle <- handleGuess (Puzzle "puzzle" [Nothing, Nothing, Just 'z', Just 'z', Nothing, Nothing] ['z'] 0) 's'
          newPuzzle `shouldBe` (Puzzle "puzzle" [Nothing, Nothing, Just 'z', Just 'z', Nothing, Nothing] ['s', 'z'] 1)
