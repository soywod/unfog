module DurationSpec
  ( spec
  )
where

import           Prelude                 hiding ( min )
import           Test.Hspec

import           Duration

spec :: Spec
spec = parallel $ do
  it "should show full duration" $ do
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

  it "should show full relative duration" $ do
    showFullRelDuration (0 * sec) `shouldBe` ""
    showFullRelDuration (59 * sec) `shouldBe` ""
    showFullRelDuration (1 * min) `shouldBe` "in 1 min"
    showFullRelDuration (-2 * min) `shouldBe` "2 mins ago"
    showFullRelDuration (-1 * hour) `shouldBe` "1 hour ago"
    showFullRelDuration (2 * hour) `shouldBe` "in 2 hours"
    showFullRelDuration (1 * day) `shouldBe` "in 1 day"
    showFullRelDuration (-2 * day) `shouldBe` "2 days ago"
    showFullRelDuration (1 * year) `shouldBe` "in 1 year"
    showFullRelDuration (-2 * year) `shouldBe` "2 years ago"
    showFullRelDuration (1 * year + 2 * day + 3 * hour + 4 * min)
      `shouldBe` "in 1 year 2 days 3 hours 4 mins"

  it "should show approx duration" $ do
    showApproxDuration (0 * sec) `shouldBe` ""
    showApproxDuration (59 * sec) `shouldBe` ""
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

  it "should show approx relative duration" $ do
    showApproxRelDuration (0 * sec) `shouldBe` ""
    showApproxRelDuration (59 * sec) `shouldBe` ""
    showApproxRelDuration (1 * min) `shouldBe` "in 1 min"
    showApproxRelDuration (-2 * min) `shouldBe` "2 mins ago"
    showApproxRelDuration (-1 * hour) `shouldBe` "1 hour ago"
    showApproxRelDuration (2 * hour) `shouldBe` "in 2 hours"
    showApproxRelDuration (1 * day) `shouldBe` "in 1 day"
    showApproxRelDuration (-2 * day) `shouldBe` "2 days ago"
    showApproxRelDuration (-1 * year) `shouldBe` "1 year ago"
    showApproxRelDuration (2 * year) `shouldBe` "in 2 years"
    showApproxRelDuration (3 * hour + 4 * min) `shouldBe` "in 3 hours"
    showApproxRelDuration (-2 * day - 3 * hour - 4 * min)
      `shouldBe` "2 days ago"
    showApproxRelDuration (1 * year + 2 * day + 3 * hour + 4 * min)
      `shouldBe` "in 1 year"
