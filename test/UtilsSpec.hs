module UtilsSpec (spec) where

import Test.Hspec
import Unfog.Utils.List as List

spec :: Spec
spec = parallel $ do
  describe "List" $ do
    it "zipProduct" $ do
      let a = ["a", "b", "c"]
      let b = [1, 2]
      let expectation = [("a", 1), ("a", 2), ("b", 1), ("b", 2), ("c", 1), ("c", 2)]
      List.zipProduct a b `shouldBe` expectation
