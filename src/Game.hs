module Game where

import Survivor

data GameStatus = InProgress | Ended deriving (Eq, Show)

data Game = Game
  {survivors :: [Survivor], status :: GameStatus, gameLevel :: Level}
  deriving (Eq, Show)

defaultGame = Game {survivors = [], status = InProgress, gameLevel = Blue}

addSurvivor :: Game -> Survivor -> Game
addSurvivor g s
  | name s `elem` map name (survivors g) = g
  | otherwise = g {survivors = s : survivors g}

normalizeStatus :: Game -> Game
normalizeStatus g
  | null (survivors g) = g {status = InProgress}
  | foldl (||) False (map alive (survivors g)) = g {status = InProgress}
  | otherwise = g {status = Ended}

normalizeGameLevel :: Game -> Game
normalizeGameLevel g = g {gameLevel = maximum [level s | s <- survivors g, alive s]}
