module TaskSpec
  ( spec
  )
where

import           Data.Time
import           Test.Hspec

import           Task

spec :: Spec
spec = parallel $ do
  let emptyActiveTask = emptyTask { _active = 1 }
  describe "getWtimePerDay" $ do
    let d10 = read "2019-12-22 10:00:00" :: UTCTime
    let d12 = read "2019-12-22 12:00:00" :: UTCTime
    let d14 = read "2019-12-22 14:00:00" :: UTCTime
    let d16 = read "2019-12-22 16:00:00" :: UTCTime
    let d18 = read "2019-12-22 18:00:00" :: UTCTime

    it "a" $ do
      let tasks = [emptyActiveTask { _starts = [d10, d14], _stops = [d12] }]
      getWtimePerDay d18 Nothing Nothing tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2 + 60 * 60 * 4)]

    it "ab" $ do
      let tasks = [emptyTask { _starts = [d10, d14], _stops = [d12, d16] }]
      getWtimePerDay d18 Nothing Nothing tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 4)]

    it "[a" $ do
      let tasks = [emptyActiveTask { _starts = [d16] }]
      getWtimePerDay d18 (Just d10) Nothing tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2)]

    it "a[" $ do
      let tasks = [emptyActiveTask { _starts = [d10] }]
      getWtimePerDay d18 (Just d14) Nothing tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 4)]

    it "[ab" $ do
      let tasks = [emptyActiveTask { _starts = [d14], _stops = [d16] }]
      getWtimePerDay d18 (Just d10) Nothing tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2)]

    it "a[b" $ do
      let tasks = [emptyActiveTask { _starts = [d12], _stops = [d16] }]
      getWtimePerDay d18 (Just d14) Nothing tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2)]

    it "ab[" $ do
      let tasks = [emptyActiveTask { _starts = [d14], _stops = [d16] }]
      getWtimePerDay d18 (Just d18) Nothing tasks `shouldBe` []

    it "]a" $ do
      let tasks = [emptyActiveTask { _starts = [d14] }]
      getWtimePerDay d18 Nothing (Just d10) tasks `shouldBe` []

    it "a]" $ do
      let tasks = [emptyActiveTask { _starts = [d10] }]
      getWtimePerDay d18 Nothing (Just d12) tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2)]

    it "]ab" $ do
      let tasks = [emptyActiveTask { _starts = [d14], _stops = [d16] }]
      getWtimePerDay d18 Nothing (Just d10) tasks `shouldBe` []

    it "a]b" $ do
      let tasks = [emptyActiveTask { _starts = [d12], _stops = [d16] }]
      getWtimePerDay d18 Nothing (Just d14) tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2)]

    it "ab]" $ do
      let tasks = [emptyActiveTask { _starts = [d14], _stops = [d16] }]
      getWtimePerDay d18 Nothing (Just d18) tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2)]

    it "[ab]" $ do
      let tasks = [emptyActiveTask { _starts = [d12], _stops = [d16] }]
      getWtimePerDay d18 (Just d10) (Just d18) tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 4)]

    it "a[b]" $ do
      let tasks = [emptyActiveTask { _starts = [d12], _stops = [d16] }]
      getWtimePerDay d18 (Just d14) (Just d18) tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2)]

    it "[a]b" $ do
      let tasks = [emptyActiveTask { _starts = [d12], _stops = [d18] }]
      getWtimePerDay d18 (Just d10) (Just d16) tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 4)]

    it "ab[]" $ do
      let tasks = [emptyActiveTask { _starts = [d10], _stops = [d14] }]
      getWtimePerDay d18 (Just d16) (Just d18) tasks `shouldBe` []

    it "[]ab" $ do
      let tasks = [emptyActiveTask { _starts = [d16], _stops = [d18] }]
      getWtimePerDay d18 (Just d10) (Just d12) tasks `shouldBe` []

    it "a[]b" $ do
      let tasks = [emptyActiveTask { _starts = [d10], _stops = [d18] }]
      getWtimePerDay d18 (Just d14) (Just d16) tasks
        `shouldBe` [("2019-12-22", 60 * 60 * 2)]
