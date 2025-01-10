-- | FileSpec

module FileSpec where

import Test.Hspec
import Files (Content(Audio), readAudioName)

spec = hspec $ do
  describe "readAudioName" $ do
    it "doesn't fail on empty name" $ do
      readAudioName "" 2 `shouldBe` (Audio Nothing "" 2)
