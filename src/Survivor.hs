module Survivor where

type Equipment = String

data Survivor = Survivor
  { name :: String,
    wounds :: Int,
    actionsRemaining :: Int,
    alive :: Bool,
    reserve :: [Equipment],
    hands :: [Equipment]
  }
  deriving (Eq, Show)

-- ACTIONS
--  - namedSurvivor
--  - wound
--  - pickUp
--  - equip
--  - unEquip

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

-- HELPERS

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

defaultSurvivor =
  Survivor
    { name = "default",
      wounds = 0,
      actionsRemaining = 3,
      alive = True,
      reserve = [],
      hands = []
    }
