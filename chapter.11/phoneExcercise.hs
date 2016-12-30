module PhoneExcercise where
  import Data.Char

  type Digit = Char
  type Presses = Int
  data Phone = Phone [(Digit, String)]

  phone :: Phone
  phone = Phone [
    ('1', "1"),
    ('2', "abc2"),
    ('3', "def3"),
    ('4', "ghi4"),
    ('5', "jkl5"),
    ('6', "mno6"),
    ('7', "pqrs7"),
    ('8', "tuv8"),
    ('9', "wxyz9"),
    ('*', "*^"),
    ('0', "+_0"),
    ('#', ".,#")
    ]

  find :: (a -> Bool) -> [a] -> a
  find f = head . filter f

  findIndex :: (a -> Bool) -> [a] -> Int
  findIndex f xs = snd $ find (\(x', _) -> f x') $ zip xs [1..]

  getKeyByDigit :: Phone -> Digit -> (Digit, String)
  getKeyByDigit (Phone keys) x = find isDigitPressed keys where
    isDigitPressed (x', _) = x == x'

  convertLetter :: Char -> Char
  convertLetter x
   | x == ' ' = '_'
   | otherwise = x

  getKeyByLetter :: Phone -> Char -> (Digit, String)
  getKeyByLetter (Phone keys) x = find isLetterPressed keys where
    isLetterPressed (_, letters) = any (==x) letters

  reverseTaps :: Phone -> Char -> [(Digit, Presses)]
  reverseTaps phone x
    | isUpper x = [('*' :: Digit, 1 :: Presses), digitPresses]
    | otherwise = [digitPresses]
    where
      key :: (Digit, String)
      key = getKeyByLetter phone ((convertLetter . toLower) x)
      digitPresses :: (Digit, Presses)
      digitPresses = (fst key, (findIndex (==((convertLetter . toLower) x)) (snd key)) :: Presses)

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

  groupBy :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
  groupBy _ [] = []
  groupBy f (x : xs) = (f x, sameKeys) : (groupBy f differentKeys) where
    sameKeys = filter (\x' -> (f x') == (f x)) xs
    differentKeys = filter (\x' -> (f x') /= (f x)) xs

  groupByLetter :: String -> [(Digit, [(Digit, Presses)])]
  groupByLetter xs = groupBy (\(x', _) -> x') (cellPhonesDead phone xs)

  tapsByLetter :: String -> [(Digit, Presses)]
  tapsByLetter xs = map f (groupByLetter xs) where
    f (x, px) = (x, fingerTaps px)

  -- mostPopularLetter :: String -> Char
  -- mostPopularLetter xs = groupByLetter xs
