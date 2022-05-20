import Survivor
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  -- Step 1: Survivors
  --
  -- At this point the differences between the functional and OO
  -- approaches is kind of superficial. `newSurvivor` almost looks
  -- like a constructor of a Survivor class. In the functional
  -- version, Survivor is a _type_ instead of a class.

  describe "newSurvivor" $ do
    it "applies the given name" $ do
      name (newSurvivor "Chad") `shouldBe` "Chad"

    it "new zombie has no wounds" $ do
      wounds (newSurvivor "Chad") `shouldBe` 0

    it "new zombie has 3 remaining actions" $ do
      actionsRemaining (newSurvivor "Chad") `shouldBe` 3
