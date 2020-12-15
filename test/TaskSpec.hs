module TaskSpec (spec) where

import Data.Time (UTCTime)
import Test.Hspec
import Unfog.Task as Task

spec :: Spec
spec = parallel $ do
  it "test" $ do
    let now = read "2020-01-01 00:00:00 UTC" :: UTCTime
    let due = read "2020-01-01 00:05:00 UTC" :: UTCTime
    let task = Task.new {_due = Just due}
    matchingReminders now [] task `shouldBe` False
    matchingReminders now [5] task `shouldBe` True
