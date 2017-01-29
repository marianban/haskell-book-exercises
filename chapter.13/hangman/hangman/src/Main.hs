module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0 , (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int
instance Show Puzzle where
  show (Puzzle _ discovered guessed missed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed ++ " Wrong guesses: " ++ (show missed)

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (f xs) [] 0 where
  f = fmap (const Nothing)

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle xs _ _ _) x = elem x xs

allreadyGuessed :: Puzzle -> Char -> Bool
allreadyGuessed (Puzzle _ xs _ _) x = (elem x) . (fmap (\(Just x') -> x')) $ filter isJust xs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

fillInCharacter :: Puzzle -> Char -> Int -> Puzzle
fillInCharacter (Puzzle word fillInSoFar s missed) c missIncrement =
  Puzzle word newFilledInSoFar (c : s) (missed + missIncrement) where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar =
      zipWith (zipper c) word fillInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, allreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in word, filling in the word accordingly"
      return (fillInCharacter puzzle guess 0)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again"
      return (fillInCharacter puzzle guess 1)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ missed) =
  if missed >= (length wordToGuess) then
    do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
  else return()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do
      putStrLn "You win!"
      exitSuccess
  else return()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be as single character"

main :: IO ()
main = do
  -- word <- randomWord'
  let word = "test"
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
