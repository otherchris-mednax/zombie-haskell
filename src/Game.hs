module Game where

import Survivor (Survivor, alive, name)

data Game = Game
  {survivors :: [Survivor], status :: String}
  deriving (Eq, Show)

defaultGame = Game {survivors = [], status = "In Progress"}

addSurvivor :: Game -> Survivor -> Game
addSurvivor g s
  | name s `elem` map name (survivors g) = g
  | otherwise = g {survivors = s : survivors g}

normalizeGame :: Game -> Game
normalizeGame g
  | null (survivors g) = g {status = "In Progress"}
  | foldl (||) False (map alive (survivors g)) = g {status = "In Progress"}
  | otherwise = g {status = "Ended"}
