module TaskSpec
  ( spec,
  )
where

import Data.Time
import Task
import Test.Hspec

spec :: Spec
spec = parallel $ do
  let emptyActiveTask = emptyTask {_id = 1, _desc = "desc", _active = 1}
  let emptyTask' = emptyActiveTask {_active = 0}
  describe "getWtimePerDay" $ do
    let d10 = read "2019-12-22 10:00:00 UTC" :: UTCTime
    let d12 = read "2019-12-22 12:00:00 UTC" :: UTCTime
    let d14 = read "2019-12-22 14:00:00 UTC" :: UTCTime
    let d16 = read "2019-12-22 16:00:00 UTC" :: UTCTime
    let d18 = read "2019-12-22 18:00:00 UTC" :: UTCTime

    it "start" $ do
      let tasks = [emptyActiveTask {_starts = [d10, d16], _stops = [d12]}]
      getWtimePerDay d18 Nothing Nothing tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 4]))]

    it "start stop" $ do
      let tasks = [emptyTask' {_starts = [d10, d14], _stops = [d12, d16]}]
      getWtimePerDay d18 Nothing Nothing tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 4]))]

    it "[ start" $ do
      let tasks = [emptyActiveTask {_starts = [d16]}]
      getWtimePerDay d18 (Just d10) Nothing tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 2]))]

    it "start [" $ do
      let tasks = [emptyActiveTask {_starts = [d10]}]
      getWtimePerDay d18 (Just d14) Nothing tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 4]))]

    it "[ start stop" $ do
      let tasks = [emptyActiveTask {_starts = [d14], _stops = [d16]}]
      getWtimePerDay d18 (Just d10) Nothing tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 2]))]

    it "start [ stop" $ do
      let tasks = [emptyActiveTask {_starts = [d12], _stops = [d16]}]
      getWtimePerDay d18 (Just d14) Nothing tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 2]))]

    it "start stop [" $ do
      let tasks = [emptyActiveTask {_starts = [d14], _stops = [d16]}]
      getWtimePerDay d18 (Just d18) Nothing tasks `shouldBe` []

    it "] start" $ do
      let tasks = [emptyActiveTask {_starts = [d14]}]
      getWtimePerDay d18 Nothing (Just d10) tasks `shouldBe` []

    it "start ]" $ do
      let tasks = [emptyActiveTask {_starts = [d10]}]
      getWtimePerDay d18 Nothing (Just d12) tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 2]))]

    it "] start stop" $ do
      let tasks = [emptyActiveTask {_starts = [d14], _stops = [d16]}]
      getWtimePerDay d18 Nothing (Just d10) tasks `shouldBe` []

    it "start ] stop" $ do
      let tasks = [emptyActiveTask {_starts = [d12], _stops = [d16]}]
      getWtimePerDay d18 Nothing (Just d14) tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 2]))]

    it "start stop ]" $ do
      let tasks = [emptyActiveTask {_starts = [d14], _stops = [d16]}]
      getWtimePerDay d18 Nothing (Just d18) tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 2]))]

    it "[ start stop ]" $ do
      let tasks = [emptyActiveTask {_starts = [d12], _stops = [d16]}]
      getWtimePerDay d18 (Just d10) (Just d18) tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 4]))]

    it "start [ stop ]" $ do
      let tasks = [emptyActiveTask {_starts = [d12], _stops = [d16]}]
      getWtimePerDay d18 (Just d14) (Just d18) tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 2]))]

    it "[ start ] stop" $ do
      let tasks = [emptyActiveTask {_starts = [d12], _stops = [d18]}]
      getWtimePerDay d18 (Just d10) (Just d16) tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 4]))]

    it "start stop []" $ do
      let tasks = [emptyActiveTask {_starts = [d10], _stops = [d14]}]
      getWtimePerDay d18 (Just d16) (Just d18) tasks `shouldBe` []

    it "[] start stop" $ do
      let tasks = [emptyActiveTask {_starts = [d16], _stops = [d18]}]
      getWtimePerDay d18 (Just d10) (Just d12) tasks `shouldBe` []

    it "start [] stop" $ do
      let tasks = [emptyActiveTask {_starts = [d10], _stops = [d18]}]
      getWtimePerDay d18 (Just d14) (Just d16) tasks
        `shouldBe` [("2019-12-22", ([WtimeTask 1 "desc" $ 60 * 60 * 2]))]
