module Survivor where

type Equipment = String

data Level = Blue | Yellow | Orange | Red deriving (Ord, Eq, Show)

data Survivor = Survivor
  { name :: String,
    wounds :: Int,
    actionsRemaining :: Int,
    alive :: Bool,
    reserve :: [Equipment],
    hands :: [Equipment],
    experience :: Int,
    level :: Level
  }
  deriving (Eq, Show)

-- ACTIONS
--  - namedSurvivor
--  - wound
--  - pickUp
--  - equip
--  - unEquip
--  - killZombie

namedSurvivor :: String -> Survivor
namedSurvivor name = defaultSurvivor {name = name}

wound :: Survivor -> Survivor
wound = normalizeInventory . wound'

pickUp :: Survivor -> Equipment -> Survivor
pickUp s item
  | inventoryCapacity s > 0 = s {reserve = item : reserve s}
  | otherwise = s

equip :: Survivor -> Equipment -> Survivor
equip s item
  | (length . hands) s >= 2 = s
  | item `notElem` reserve s = s
  | otherwise = s {hands = item : hands s, reserve = [x | x <- reserve s, x /= item]}

unEquip :: Survivor -> Equipment -> Survivor
unEquip s item
  | item `notElem` hands s = s
  | otherwise = s {reserve = item : reserve s, hands = [x | x <- hands s, x /= item]}

killZombie :: Survivor -> Survivor
killZombie s = normalizeLevel s {experience = experience s + 1}

-- HELPERS
--  - inventoryCapacity
--  - normalizeInventory
--  - wound'
--  - normalizeLevel
--  - defaultSurvivor

inventoryCapacity :: Survivor -> Int
inventoryCapacity s = 5 - (length . reserve) s - (length . hands) s - wounds s

normalizeInventory :: Survivor -> Survivor
normalizeInventory s
  | inventoryCapacity s >= 0 = s
  | otherwise = normalizeInventory s {reserve = (tail . reserve) s}

wound' :: Survivor -> Survivor
wound' s
  | wounds s == 0 = s {wounds = 1}
  | wounds s == 1 = s {wounds = 2, alive = False}
  | otherwise = s

normalizeLevel :: Survivor -> Survivor
normalizeLevel s
  | experience s > 42 = s {level = Red}
  | experience s > 18 = s {level = Orange}
  | experience s > 6 = s {level = Yellow}
  | otherwise = s {level = Blue}

defaultSurvivor =
  Survivor
    { name = "default",
      wounds = 0,
      actionsRemaining = 3,
      alive = True,
      reserve = [],
      hands = [],
      experience = 0,
      level = Blue
    }
