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

  describe "normalizeStatus" $ do
    let s = defaultSurvivor
        g = defaultGame
    it "ends the game if all players are dead (>0 players)" $ do
      let g' = g {survivors = [s {alive = False}]}
      (status . normalizeStatus) g' `shouldBe` Ended

    it "game with no players is in progress" $ do
      let g' = g {status = Ended}
      (status . normalizeStatus) g' `shouldBe` InProgress

    it "game with a living player is in progress" $ do
      let g' = g {survivors = [s {alive = False}, s]}
      (status . normalizeStatus) g' `shouldBe` InProgress

  describe "normalizeGameLevel" $ do
    let s = defaultSurvivor
        g = defaultGame
    it "sets the game level at the highest living survivor level" $ do
      let g' = g {survivors = [s {level = Blue}, s {alive = False, level = Red}, s {level = Orange}]}
      (gameLevel . normalizeGameLevel) g' `shouldBe` Orange
