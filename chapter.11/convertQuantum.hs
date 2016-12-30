module ConvertQuantum where
  data Quantum = Yes | No | Both deriving (Eq, Show)
  convert :: Quantum -> Bool
  convert Yes = True
  convert No = True
  convert Both = True

  convert2 :: Quantum -> Bool
  convert Yes = True
  convert No = True
  convert Both = False

  convert3 :: Quantum -> Bool
  convert Yes = True
  convert No = False
  convert Both = False

  convert4 :: Quantum -> Bool
  convert Yes = False
  convert No = False
  convert Both = False

  convert5 :: Quantum -> Bool
  convert Yes = False
  convert No = False
  convert Both = True

  convert6 :: Quantum -> Bool
  convert Yes = False
  convert No = True
  convert Both = True

  convert7 :: Quantum -> Bool
  convert Yes = False
  convert No = True
  convert Both = False

  convert8 :: Quantum -> Bool
  convert Yes = True
  convert No = False
  convert Both = True
