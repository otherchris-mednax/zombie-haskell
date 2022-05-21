module Survivor where

data Survivor = Survivor
  { name :: String,
    wounds :: Int,
    actionsRemaining :: Int,
    alive :: Bool
  }
  deriving (Eq, Show)

defaultSurvivor = Survivor {name = "default", wounds = 0, actionsRemaining = 3, alive = True}

newSurvivor :: String -> Survivor
newSurvivor name = defaultSurvivor {name = name}

wound :: Survivor -> Survivor
wound s
  | wounds s == 0 = s {wounds = 1}
  | wounds s == 1 = s {wounds = 2, alive = False}
  | otherwise = s
