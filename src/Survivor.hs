module Survivor where

data Survivor = Survivor
  { name :: String,
    wounds :: Int,
    actionsRemaining :: Int
  }

newSurvivor :: String -> Survivor
newSurvivor name = Survivor {name = name, wounds = 0, actionsRemaining = 3}
