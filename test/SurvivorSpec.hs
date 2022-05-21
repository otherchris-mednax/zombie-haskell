module SurvivorSpec where

import Survivor
import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "namedSurvivor" $ do
    it "applies the given name" $ do
      (name . namedSurvivor) "Chad" `shouldBe` "Chad"

    it "new survivor has no wounds" $ do
      (wounds . namedSurvivor) "Chad" `shouldBe` 0

    it "new survivor has 3 remaining actions" $ do
      (actionsRemaining . namedSurvivor) "Chad" `shouldBe` 3

  -- ACTIONS

  describe "wound" $ do
    let s = defaultSurvivor
    it "increases the wounds of a Survivor by one" $ do
      (wounds . wound) s `shouldBe` 1

    it "does not increase wounds past 2" $ do
      let s' = s {wounds = 2}
      wound s' `shouldBe` s'

    it "causes death if wounds is to be 2" $ do
      let s' = s {wounds = 1}
      (alive . wound) s' `shouldBe` False

    it "normalizes inventory if needed" $ do
      let s' = s {reserve = ["Paintbrush", "Steel Cut Oats", "Mysterious Key", "Can", "Sharpie"]}
      (length . reserve . wound) s' `shouldBe` 4

  describe "pickUp" $ do
    let s = defaultSurvivor
    it "adds an item to reserve if possible" $ do
      pickUp s "Belt Sander" `shouldBe` s {reserve = ["Belt Sander"]}

    it "does not add the item if not possible" $ do
      let s' = s {wounds = 1, hands = ["Flamberge", "Diet Coke"], reserve = ["Ice Beam", "Handful of Sand"]}
      pickUp s' "Mona Lisa" `shouldBe` s'

  describe "equip" $ do
    let s = defaultSurvivor {reserve = ["Battleship"]}
    it "moves an item to the hands" $ do
      equip s "Battleship" `shouldBe` s {hands = ["Battleship"], reserve = []}

    it "doesn't move it if the hands are full" $ do
      let s' = s {hands = ["Clowns", "Jokers"]}
      equip s' "Battleship" `shouldBe` s'

    it "doesn't change anything if the item is not in reserve" $ do
      equip s "Capybara" `shouldBe` s

  describe "unEquip" $ do
    let s = defaultSurvivor {hands = ["Orb", "Big Mac"]}
    it "moves an item to reserve if possible" $ do
      unEquip s "Orb" `shouldBe` s {reserve = ["Orb"], hands = ["Big Mac"]}

    it "doesn't move it if it's not in the hands" $ do
      unEquip s "Zero Point Energy Field Manipulator" `shouldBe` s

  -- HELPERS

  describe "inventoryCapacity" $ do
    it "tells the amount of inventory left" $ do
      inventoryCapacity defaultSurvivor `shouldBe` 5

      let s = defaultSurvivor {reserve = ["Sponge"]}
      inventoryCapacity s `shouldBe` 4

      let s = defaultSurvivor {reserve = ["Sponge"], hands = ["Lampshade"]}
      inventoryCapacity s `shouldBe` 3

      let s = defaultSurvivor {reserve = ["Sponge", "A", "B", "4", "waffle", "oops all crunchberries"]}
      inventoryCapacity s `shouldBe` -1

      let s = defaultSurvivor {reserve = ["Sponge"], wounds = 1}
      inventoryCapacity s `shouldBe` 3

  describe "normalizeInventory" $ do
    let s = defaultSurvivor {wounds = 1, reserve = ["Too", "Much", "Stuff", "Up"], hands = ["In", "Here"]}
    it "gets the inventory down to capacity, dropping from reserve" $ do
      (length . reserve . normalizeInventory) s `shouldBe` 2
      (length . hands . normalizeInventory) s `shouldBe` 2

    it "doesn't affect a survivor at or below capacity" $ do
      let s' = s {hands = []}
      normalizeInventory s' `shouldBe` s'

