module PhoneExcercise where
  import Data.Char
  import Data.List

  type Digit = Char
  type Presses = Int
  data Key = Key { digit :: Digit, letters :: String } deriving (Show, Eq)

  data Phone = Phone [Key] deriving (Show, Eq)

  phone :: Phone
  phone = Phone [
    Key { digit = '1', letters = "1" },
    Key { digit = '2', letters = "abc2" },
    Key { digit = '3', letters = "def3" },
    Key { digit = '4', letters = "ghi4" },
    Key { digit = '5', letters = "jkl5" },
    Key { digit = '6', letters = "mno6" },
    Key { digit = '7', letters = "pqrs7" },
    Key { digit = '8', letters = "tuv8" },
    Key { digit ='9', letters = "wxyz9" },
    Key { digit = '*', letters = "*^" },
    Key { digit = '0', letters = "+_0" },
    Key { digit = '#', letters = ".,#" }
    ]

  myFind :: (a -> Bool) -> [a] -> a
  myFind f = head . filter f

  myFindIndex :: (a -> Bool) -> [a] -> Int
  myFindIndex f xs = snd $ myFind (\(x', _) -> f x') $ zip xs [1..]

  getKeyByDigit :: Phone -> Digit -> Key
  getKeyByDigit (Phone keys) x = myFind isDigitPressed keys where
    isDigitPressed key = x == digit key

  convertLetter :: Char -> Char
  convertLetter x
   | x == ' ' = '_'
   | otherwise = x

  getKeyByLetter :: Phone -> Char -> Key
  getKeyByLetter (Phone keys) x = myFind isLetterPressed keys where
    isLetterPressed key = any (==x) (letters key)

  reverseTaps :: Phone -> Char -> [(Digit, Presses)]
  reverseTaps phone x
    | isUpper x = [('*' :: Digit, 1 :: Presses), digitPresses]
    | otherwise = [digitPresses]
    where
      key :: Key
      key = getKeyByLetter phone ((convertLetter . toLower) x)
      digitPresses :: (Digit, Presses)
      digitPresses = (digit key, (myFindIndex (==((convertLetter . toLower) x)) (letters key)) :: Presses)

  cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
  cellPhonesDead phone = foldr (\x acc -> acc ++ (reverseTaps phone x)) []

  convo :: [String]
  convo = ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

  conversation = map (cellPhonesDead phone) convo

  fingerTaps :: [(Digit, Presses)] -> Presses
  fingerTaps xs = (foldr (\(_, p) acc -> acc + p) 0 xs) :: Presses

  myGroupBy :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
  myGroupBy _ [] = []
  myGroupBy f xs@(x : _) = (f x, sameKeys) : (myGroupBy f differentKeys) where
    sameKeys = filter (\x' -> (f x') == (f x)) xs
    differentKeys = filter (\x' -> (f x') /= (f x)) xs

  groupByLetter :: String -> [(Digit, [(Digit, Presses)])]
  groupByLetter xs = myGroupBy (\(x', _) -> x') (cellPhonesDead phone xs)

  tapsByLetter :: String -> [(Digit, Presses)]
  tapsByLetter xs = map f (groupByLetter xs) where
    f (x, px) = (x, fingerTaps px)

  mostPopularLetter :: String -> Char
  mostPopularLetter xs = fst $ maximumBy (\x y -> compare (snd x) (snd y)) (tapsByLetter xs)
