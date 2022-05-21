module GameSpec where

import Game
import Survivor
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "addSurvivor" $ do
    let g = defaultGame {survivors = [namedSurvivor "Chad"]}
    it "adds a survivor if the name is unique" $ do
      let g' = addSurvivor g (namedSurvivor "Buffy")
      (length . survivors) g' `shouldBe` 2

    it "doesn't add survivor if the name is not unique" $ do
      let g' = addSurvivor g (namedSurvivor "Chad")
      (length . survivors) g' `shouldBe` 1

  describe "normalizeGame" $ do
    let s = defaultSurvivor
        g = defaultGame
    it "ends the game if all players are dead (>0 players)" $ do
      let g' = g {survivors = [s {alive = False}]}
      (status . normalizeGame) g' `shouldBe` "Ended"

    it "game with no players is in progress" $ do
      let g' = g {status = "Boiled Chicken"}
      (status . normalizeGame) g' `shouldBe` "In Progress"

    it "game with a living player is in progress" $ do
      let g' = g {survivors = [s {alive = False}, s]}
      (status . normalizeGame) g' `shouldBe` "In Progress"
