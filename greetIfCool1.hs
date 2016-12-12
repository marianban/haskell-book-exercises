module GreatIfCool1 where
  greatIfCool :: String -> IO()
  greatIfCool coolness =
    if coolness == "downright frosty yo"
      then
        putStrLn "eyyyy. Whats shakin?"
      else
        putStrLn "pshhh"
      where
        cool = coolness == "downright frosty yo"
