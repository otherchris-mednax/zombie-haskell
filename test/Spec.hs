import Survivor
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "newSurvivor" $ do
    it "applies the given name" $ do
      (name . newSurvivor) "Chad" `shouldBe` "Chad"

    it "new survivor has no wounds" $ do
      (wounds . newSurvivor) "Chad" `shouldBe` 0

    it "new survivor has 3 remaining actions" $ do
      (actionsRemaining . newSurvivor) "Chad" `shouldBe` 3

  describe "wound" $ do
    it "increases the wounds of a Survivor by one" $ do
      (wounds . wound . newSurvivor) "Chad" `shouldBe` 1

    it "does not increase wounds past 2" $ do
      let s = newSurvivor "Chad"
          s' = s {wounds = 2}
      wound s' `shouldBe` s'

    it "causes death if wounds is to be 2" $ do
      let s = newSurvivor "Chad"
          s' = s {wounds = 1}
      (alive . wound) s' `shouldBe` False
