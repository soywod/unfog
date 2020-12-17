module StateSpec (spec) where

import Data.Time (UTCTime)
import Test.Hspec
import Unfog.Event.Type (Event (..))
import Unfog.State as State
import Unfog.Task (Task (..))
import qualified Unfog.Task as Task

spec :: Spec
spec = parallel $ do
  let now = read "2020-01-01 00:00:00 UTC" :: UTCTime
  let tomorrow = read "2020-01-02 00:00:00 UTC" :: UTCTime

  it "rebuild" $ do
    rebuild [] `shouldBe` State.new
    rebuild
      [ TaskAdded now "id" "desc" Nothing Nothing
      ]
      `shouldBe` State.new {_tasks = [Task.new {_id = "id", _desc = "desc"}]}
    rebuild
      [ TaskAdded now "id" "desc" (Just "proj") Nothing
      ]
      `shouldBe` State.new {_tasks = [Task.new {_id = "id", _desc = "desc", _project = Just "proj"}]}
    rebuild
      [ TaskAdded now "id" "desc" Nothing (Just now)
      ]
      `shouldBe` State.new {_tasks = [Task.new {_id = "id", _desc = "desc", _due = Just now}]}
    rebuild
      [ TaskAdded now "id" "desc" Nothing Nothing,
        TaskEdited now "id" "desc" (Just "proj") Nothing
      ]
      `shouldBe` State.new {_tasks = [Task.new {_id = "id", _desc = "desc", _project = Just "proj"}]}
    rebuild
      [ TaskAdded now "id" "desc" Nothing Nothing,
        TaskStarted now "id"
      ]
      `shouldBe` State.new {_tasks = [Task.new {_id = "id", _desc = "desc", _active = Just now, _starts = [now]}]}
    rebuild
      [ TaskAdded now "id" "desc" Nothing Nothing,
        TaskStarted now "id",
        TaskStopped tomorrow "id"
      ]
      `shouldBe` State.new {_tasks = [Task.new {_id = "id", _desc = "desc", _starts = [now], _stops = [tomorrow]}]}
    rebuild
      [ TaskAdded now "id" "desc" Nothing Nothing,
        TaskStarted now "id",
        TaskDid tomorrow "id"
      ]
      `shouldBe` State.new {_tasks = [Task.new {_id = "id", _desc = "desc", _starts = [now], _stops = [tomorrow], _done = Just tomorrow}]}
    rebuild
      [ TaskAdded now "id" "desc" Nothing Nothing,
        TaskStarted now "id",
        TaskDeleted tomorrow "id"
      ]
      `shouldBe` State.new {_tasks = [Task.new {_id = "id", _desc = "desc", _starts = [now], _stops = [tomorrow], _deleted = Just tomorrow}]}
    rebuild
      [ ContextEdited now (Just "proj")
      ]
      `shouldBe` State.new {_ctx = Just "proj"}
