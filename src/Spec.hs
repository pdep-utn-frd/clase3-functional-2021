module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "sumatoria [1,2,3] es 6" $ do
        sumatoria [1,2,3] `shouldBe` 6
    it "sumatoria [] es 0" $ do
        sumatoria [] `shouldBe` 0
    it "sumatoria [4] es 4" $ do
        sumatoria [4] `shouldBe` 4

    it "duplicarElementos [1,2,3] es [2,4,6]" $ do
        duplicarElementos [1,2,3] `shouldBe` [2,4,6]
    it "duplicarElementos [3] es [6]" $ do
        duplicarElementos [3] `shouldBe` [6]
    it "duplicarElementos [] es []" $ do
        duplicarElementos [] `shouldBe` []
