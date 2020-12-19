module QuerySpec (spec) where

import Data.Time (UTCTime)
import Test.Hspec
import qualified Unfog.Arg.Types as Arg
import Unfog.Duration (hour, sec)
import Unfog.Query.Handler (queryOfArg)
import Unfog.Query.Types
import Unfog.Response (ResponseType (..))
import Unfog.State (State (..))
import qualified Unfog.State as State
import Unfog.Task (Task (..))
import qualified Unfog.Task as Task
import Unfog.Worktime (Worktime (..))

spec :: Spec
spec = parallel $ do
  describe "parse queries" $ do
    let now = read "2020-01-01 00:00:00 UTC" :: UTCTime
    let nowPlus5min = read "2020-01-01 00:05:00 UTC" :: UTCTime

    it "parse list query" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "id-proj", _project = Just "proj"}
      let taskC = Task.new {_id = "id-done", _done = Just now}
      let taskD = Task.new {_id = "id-deleted", _deleted = Just now}
      let taskE = Task.new {_id = "id-due", _due = Just nowPlus5min}
      let queryOfArg' ctx = queryOfArg now State.new {_tasks = [taskA, taskB, taskC, taskD, taskE], _ctx = ctx}
      queryOfArg' Nothing (Arg.ShowTasks Nothing False False False) `shouldBe` ShowTasks now Text 5 Nothing [taskA, taskB, taskE]
      queryOfArg' Nothing (Arg.ShowTasks Nothing False False True) `shouldBe` ShowTasks now Json 5 Nothing [taskA, taskB, taskE]
      queryOfArg' Nothing (Arg.ShowTasks Nothing True False False) `shouldBe` ShowTasks now Text 5 Nothing [taskC]
      queryOfArg' Nothing (Arg.ShowTasks Nothing False True False) `shouldBe` ShowTasks now Text 5 Nothing [taskD]
      queryOfArg' Nothing (Arg.ShowTasks (Just 3) False False False) `shouldBe` ShowTasks now Text 5 Nothing []
      queryOfArg' Nothing (Arg.ShowTasks (Just 5) False False False) `shouldBe` ShowTasks now Text 5 Nothing [taskE]
      queryOfArg' Nothing (Arg.ShowTasks (Just 6) False False False) `shouldBe` ShowTasks now Text 5 Nothing [taskE]
      queryOfArg' (Just "proj") (Arg.ShowTasks Nothing False False False) `shouldBe` ShowTasks now Text 5 (Just "proj") [taskB]

    it "parse info query" $ do
      let task = Task.new {_id = "id"}
      let queryOfArg' = queryOfArg now State.new {_tasks = [task]}
      queryOfArg' (Arg.ShowTask "id" False) `shouldBe` ShowTask now Text task
      queryOfArg' (Arg.ShowTask "id" True) `shouldBe` ShowTask now Json task
      queryOfArg' (Arg.ShowTask "id-unknown" False) `shouldBe` Error Text "task not found"

    it "parse worktime query" $ do
      let start = read "2020-01-01 22:00:00 UTC" :: UTCTime
      let stop = read "2020-01-02 04:00:00 UTC" :: UTCTime
      let task = Task.new {_id = "id", _desc = "desc", _starts = [start], _stops = [stop]}
      let queryOfArg' tasks = queryOfArg now State.new {_tasks = tasks}
      queryOfArg' [] (Arg.ShowWorktime Nothing Nothing Nothing False False) `shouldBe` ShowWorktime now Text False []
      queryOfArg' [] (Arg.ShowWorktime Nothing Nothing Nothing False True) `shouldBe` ShowWorktime now Json False []
      queryOfArg' [] (Arg.ShowWorktime Nothing Nothing Nothing True False) `shouldBe` ShowWorktime now Text True []
      queryOfArg' [] (Arg.ShowWorktime (Just "proj") Nothing Nothing False False) `shouldBe` ShowWorktime now Text False []
      queryOfArg' [task] (Arg.ShowWorktime Nothing Nothing Nothing False False)
        `shouldBe` ShowWorktime now Text False [("2020-01-01", [Worktime "id" "desc" (2 * hour - 1 * sec)]), ("2020-01-02", [Worktime "id" "desc" (4 * hour)])]
      queryOfArg' [task] (Arg.ShowWorktime Nothing (Just (read "2020-01-02 00:00:00 UTC" :: UTCTime)) Nothing False False)
        `shouldBe` ShowWorktime now Text False [("2020-01-02", [Worktime "id" "desc" (4 * hour)])]
      queryOfArg' [task] (Arg.ShowWorktime Nothing Nothing (Just (read "2020-01-02 00:00:00 UTC" :: UTCTime)) False False)
        `shouldBe` ShowWorktime now Text False [("2020-01-01", [Worktime "id" "desc" (2 * hour - 1 * sec)])]

    it "parse status query" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "id-started", _active = Just now}
      let taskC = Task.new {_id = "id-started-bis", _active = Just now}
      let queryOfArg' tasks = queryOfArg now State.new {_tasks = tasks}
      queryOfArg' [] (Arg.ShowStatus False False) `shouldBe` ShowStatus now Text Nothing
      queryOfArg' [taskA] (Arg.ShowStatus False True) `shouldBe` ShowStatus now Json Nothing
      queryOfArg' [taskB] (Arg.ShowStatus False False) `shouldBe` ShowStatus now Text (Just taskB)
      queryOfArg' [taskB, taskC] (Arg.ShowStatus False False) `shouldBe` ShowStatus now Text (Just taskB)
