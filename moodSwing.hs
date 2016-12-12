module MoodSwing where
  data Mood = Blah | Woot deriving Show
  changeMood :: Mood -> Mood
  changeMood Blah = Woot
  changeMood _ = Blah // can be changeMood Woot = Blah
