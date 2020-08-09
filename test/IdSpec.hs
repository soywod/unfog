module IdSpec (spec) where

import Data.Time
import Task
import Test.Hspec

aTask :: Task
aTask =
  Task
    { _id = "00000000-0000-0000-0000-000000000000",
      _desc = "",
      _project = Nothing,
      _starts = [],
      _stops = [],
      _due = Nothing,
      _active = Nothing,
      _done = Nothing,
      _deleted = Nothing
    }

spec :: Spec
spec = parallel $ do
  it "Finds the minimum length of ids so that ids are non-ambigous" $ do
    getShortIdLength
      [ aTask {_id = "12345678-0000-0000-0000-000000000000"},
        aTask {_id = "00000000-0000-0000-0000-000000000000"}
      ]
      `shouldBe` 4

    getShortIdLength
      [ aTask {_id = "12345678-0000-0000-0000-000000000000"},
        aTask {_id = "00000000-0000-0000-0000-000000000000"},
        aTask {_id = "00000001-0000-0000-0000-000000000000"}
      ]
      `shouldBe` 8
