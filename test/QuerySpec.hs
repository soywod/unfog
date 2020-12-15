module QuerySpec (spec) where

import Data.Time (UTCTime)
import Test.Hspec
import qualified Unfog.ArgParser as Arg
import Unfog.Duration (hour, sec)
import Unfog.Query
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
    let idLength = 7

    it "parse list query" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "id-proj", _project = Just "proj"}
      let taskC = Task.new {_id = "id-done", _done = Just now}
      let taskD = Task.new {_id = "id-deleted", _deleted = Just now}
      let parseQuery' ctx = parseQuery now State.new {_tasks = [taskA, taskB, taskC, taskD], _ctx = ctx}
      parseQuery' Nothing (Arg.ShowTasks False False False) `shouldBe` ShowTasks now 5 Text Nothing [taskA, taskB]
      parseQuery' Nothing (Arg.ShowTasks False False True) `shouldBe` ShowTasks now 5 Json Nothing [taskA, taskB]
      parseQuery' Nothing (Arg.ShowTasks True False False) `shouldBe` ShowTasks now 5 Text Nothing [taskC]
      parseQuery' Nothing (Arg.ShowTasks False True False) `shouldBe` ShowTasks now 5 Text Nothing [taskD]
      parseQuery' (Just "proj") (Arg.ShowTasks False False False) `shouldBe` ShowTasks now 5 Text (Just "proj") [taskB]

    it "parse info query" $ do
      let task = Task.new {_id = "id"}
      let parseQuery' = parseQuery now State.new {_tasks = [task]}
      parseQuery' (Arg.ShowTask "id" False) `shouldBe` ShowTask now Text task
      parseQuery' (Arg.ShowTask "id" True) `shouldBe` ShowTask now Json task
      parseQuery' (Arg.ShowTask "id-unknown" False) `shouldBe` Error Text "task not found"

    it "parse worktime query" $ do
      let start = read "2020-01-01 22:00:00 UTC" :: UTCTime
      let stop = read "2020-01-02 04:00:00 UTC" :: UTCTime
      let task = Task.new {_id = "id", _desc = "desc", _starts = [start], _stops = [stop]}
      let parseQuery' tasks = parseQuery now State.new {_tasks = tasks}
      parseQuery' [] (Arg.ShowWorktime Nothing Nothing Nothing False False) `shouldBe` ShowWorktime now Text False []
      parseQuery' [] (Arg.ShowWorktime Nothing Nothing Nothing False True) `shouldBe` ShowWorktime now Json False []
      parseQuery' [] (Arg.ShowWorktime Nothing Nothing Nothing True False) `shouldBe` ShowWorktime now Text True []
      parseQuery' [] (Arg.ShowWorktime (Just "proj") Nothing Nothing False False) `shouldBe` ShowWorktime now Text False []
      parseQuery' [task] (Arg.ShowWorktime Nothing Nothing Nothing False False)
        `shouldBe` ShowWorktime now Text False [("2020-01-01", [Worktime "id" "desc" (2 * hour - 1 * sec)]), ("2020-01-02", [Worktime "id" "desc" (4 * hour)])]
      parseQuery' [task] (Arg.ShowWorktime Nothing (Just (read "2020-01-02 00:00:00 UTC" :: UTCTime)) Nothing False False)
        `shouldBe` ShowWorktime now Text False [("2020-01-02", [Worktime "id" "desc" (4 * hour)])]
      parseQuery' [task] (Arg.ShowWorktime Nothing Nothing (Just (read "2020-01-02 00:00:00 UTC" :: UTCTime)) False False)
        `shouldBe` ShowWorktime now Text False [("2020-01-01", [Worktime "id" "desc" (2 * hour - 1 * sec)])]

    it "parse status query" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "id-started", _active = Just now}
      let taskC = Task.new {_id = "id-started-bis", _active = Just now}
      let parseQuery' tasks = parseQuery now State.new {_tasks = tasks}
      parseQuery' [] (Arg.ShowStatus False False) `shouldBe` ShowStatus now Text Nothing
      parseQuery' [taskA] (Arg.ShowStatus False True) `shouldBe` ShowStatus now Json Nothing
      parseQuery' [taskB] (Arg.ShowStatus False False) `shouldBe` ShowStatus now Text (Just taskB)
      parseQuery' [taskB, taskC] (Arg.ShowStatus False False) `shouldBe` ShowStatus now Text (Just taskB)
