module TheAnswerIsTheExercise where
  import Control.Monad (join)

  bind :: Monad m => (a -> m b) -> m a -> m b
  bind f xs = join $ fmap f xs
