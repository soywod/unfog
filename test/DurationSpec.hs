module DurationSpec (spec) where

import Data.Time (UTCTime)
import Duration
import Test.Hspec
import Prelude hiding (min)

spec :: Spec
spec = parallel $ do
  it "units" $ do
    ms `shouldBe` 0.001000
    sec `shouldBe` 1
    min `shouldBe` 60
    hour `shouldBe` 3600
    day `shouldBe` 86400
    year `shouldBe` 31536000

  it "show units" $ do
    showMs ms `shouldBe` "1 ms"
    showMs (5 * ms) `shouldBe` "5 ms"
    showSecs sec `shouldBe` "1 sec"
    showSecs (5 * sec) `shouldBe` "5 secs"
    showMins min `shouldBe` "1 min"
    showMins (5 * min) `shouldBe` "5 mins"
    showHours hour `shouldBe` "1 hour"
    showHours (5 * hour) `shouldBe` "5 hours"
    showDays day `shouldBe` "1 day"
    showDays (5 * day) `shouldBe` "5 days"
    showYears year `shouldBe` "1 year"
    showYears (5 * year) `shouldBe` "5 years"

  it "show full duration" $ do
    showFullDuration (0 * sec) `shouldBe` ""
    showFullDuration (59 * sec) `shouldBe` ""
    showFullDuration (1 * min) `shouldBe` "1 min"
    showFullDuration (2 * min) `shouldBe` "2 mins"
    showFullDuration (1 * hour) `shouldBe` "1 hour"
    showFullDuration (2 * hour) `shouldBe` "2 hours"
    showFullDuration (1 * day) `shouldBe` "1 day"
    showFullDuration (2 * day) `shouldBe` "2 days"
    showFullDuration (1 * year) `shouldBe` "1 year"
    showFullDuration (2 * year) `shouldBe` "2 years"
    showFullDuration (1 * year + 2 * day + 3 * hour + 4 * min)
      `shouldBe` "1 year 2 days 3 hours 4 mins"

  it "show full relative duration" $ do
    showFullDurationRel (0 * sec) `shouldBe` ""
    showFullDurationRel (59 * sec) `shouldBe` ""
    showFullDurationRel (1 * min) `shouldBe` "in 1 min"
    showFullDurationRel (-2 * min) `shouldBe` "2 mins ago"
    showFullDurationRel (-1 * hour) `shouldBe` "1 hour ago"
    showFullDurationRel (2 * hour) `shouldBe` "in 2 hours"
    showFullDurationRel (1 * day) `shouldBe` "in 1 day"
    showFullDurationRel (-2 * day) `shouldBe` "2 days ago"
    showFullDurationRel (1 * year) `shouldBe` "in 1 year"
    showFullDurationRel (-2 * year) `shouldBe` "2 years ago"
    showFullDurationRel (1 * year + 2 * day + 3 * hour + 4 * min)
      `shouldBe` "in 1 year 2 days 3 hours 4 mins"

  it "show approximate duration" $ do
    showApproxDuration (0 * sec) `shouldBe` ""
    showApproxDuration (59 * sec) `shouldBe` "59 secs"
    showApproxDuration (1 * min) `shouldBe` "1 min"
    showApproxDuration (2 * min) `shouldBe` "2 mins"
    showApproxDuration (1 * hour) `shouldBe` "1 hour"
    showApproxDuration (2 * hour) `shouldBe` "2 hours"
    showApproxDuration (1 * day) `shouldBe` "1 day"
    showApproxDuration (2 * day) `shouldBe` "2 days"
    showApproxDuration (1 * year) `shouldBe` "1 year"
    showApproxDuration (2 * year) `shouldBe` "2 years"
    showApproxDuration (3 * hour + 4 * min) `shouldBe` "3 hours"
    showApproxDuration (2 * day + 3 * hour + 4 * min) `shouldBe` "2 days"
    showApproxDuration (1 * year + 2 * day + 3 * hour + 4 * min)
      `shouldBe` "1 year"

  it "show approximate relative duration" $ do
    showApproxDurationRel (0 * sec) `shouldBe` ""
    showApproxDurationRel (59 * sec) `shouldBe` "in 59 secs"
    showApproxDurationRel (1 * min) `shouldBe` "in 1 min"
    showApproxDurationRel (-2 * min) `shouldBe` "2 mins ago"
    showApproxDurationRel (-1 * hour) `shouldBe` "1 hour ago"
    showApproxDurationRel (2 * hour) `shouldBe` "in 2 hours"
    showApproxDurationRel (1 * day) `shouldBe` "in 1 day"
    showApproxDurationRel (-2 * day) `shouldBe` "2 days ago"
    showApproxDurationRel (-1 * year) `shouldBe` "1 year ago"
    showApproxDurationRel (2 * year) `shouldBe` "in 2 years"
    showApproxDurationRel (3 * hour + 4 * min) `shouldBe` "in 3 hours"
    showApproxDurationRel (-2 * day - 3 * hour - 4 * min) `shouldBe` "2 days ago"
    showApproxDurationRel (1 * year + 2 * day + 3 * hour + 4 * min) `shouldBe` "in 1 year"

  it "show micro time diff" $ do
    let timeA = read "2020-01-01 00:00:00 UTC" :: UTCTime
    let timeB = read "2020-01-02 02:20:30 UTC" :: UTCTime

    showMicroTimeDiff timeA (Just timeB) `shouldBe` day + (2 * hour) + (20 * min) + (30 * sec)
    showMicroTimeDiff timeB (Just timeA) `shouldBe` -1 * (day + (2 * hour) + (20 * min) + (30 * sec))
    showApproxTimeDiff timeA (Just timeB) `shouldBe` "1 day"
    showApproxTimeDiff timeB (Just timeA) `shouldBe` "1 day"
    showApproxTimeDiffRel timeA (Just timeB) `shouldBe` "in 1 day"
    showApproxTimeDiffRel timeB (Just timeA) `shouldBe` "1 day ago"
    showFullTimeDiff timeA (Just timeB) `shouldBe` "1 day 2 hours 20 mins"
    showFullTimeDiff timeB (Just timeA) `shouldBe` "1 day 2 hours 20 mins"
    showFullTimeDiffRel timeA (Just timeB) `shouldBe` "in 1 day 2 hours 20 mins"
    showFullTimeDiffRel timeB (Just timeA) `shouldBe` "1 day 2 hours 20 mins ago"
