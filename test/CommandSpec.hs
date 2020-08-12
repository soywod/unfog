module CommandSpec (spec) where

import qualified ArgParser as Arg
import Command
import Data.Time (TimeZone, UTCTime)
import Event.Type
import Response (ResponseType (..))
import State (State (..))
import qualified State
import Task (Task (..))
import qualified Task
import Test.Hspec
import Prelude hiding (min)

spec :: Spec
spec = parallel $ do
  let now = read "2020-01-01 00:00:00 UTC" :: UTCTime
  let tzone = read "UTC" :: TimeZone

  describe "parse commands" $ do
    it "parse add command" $ do
      let parseCommands' = parseCommands now tzone State.new "id"
      parseCommands' (Arg.Add "desc" Nothing Nothing False) `shouldBe` [AddTask now Text "id" "desc" Nothing Nothing]
      parseCommands' (Arg.Add "desc" Nothing Nothing True) `shouldBe` [AddTask now Json "id" "desc" Nothing Nothing]
      parseCommands' (Arg.Add "desc" (Just "proj") Nothing False) `shouldBe` [AddTask now Text "id" "desc" (Just "proj") Nothing]
      parseCommands' (Arg.Add "desc" Nothing (Just now) False) `shouldBe` [AddTask now Text "id" "desc" Nothing (Just now)]
      parseCommands' (Arg.Add "" Nothing Nothing False) `shouldBe` [Error Text "desc missing"]

    it "parse edit command" $ do
      let task = Task.new {_id = "id"}
      let state = State.new {_tasks = [task]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.Edit "id" "desc" Nothing Nothing False) `shouldBe` [EditTask now Text "id" "desc" Nothing Nothing]
      parseCommands' (Arg.Edit "id" "desc" Nothing Nothing True) `shouldBe` [EditTask now Json "id" "desc" Nothing Nothing]
      parseCommands' (Arg.Edit "id" "desc" (Just "proj") Nothing False) `shouldBe` [EditTask now Text "id" "desc" (Just "proj") Nothing]
      parseCommands' (Arg.Edit "id" "desc" Nothing (Just now) False) `shouldBe` [EditTask now Text "id" "desc" Nothing (Just now)]
      parseCommands' (Arg.Edit "id" "desc" Nothing (Just now) False) `shouldBe` [EditTask now Text "id" "desc" Nothing (Just now)]
      parseCommands' (Arg.Edit "id-unknown" "desc" Nothing Nothing False) `shouldBe` [Error Text "task not found"]

    it "parse start command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "id-started", _active = Just now}
      let taskC = Task.new {_id = "id-done", _done = Just now}
      let taskD = Task.new {_id = "id-deleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC, taskD]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.Start [] False) `shouldBe` []
      parseCommands' (Arg.Start ["id"] False) `shouldBe` [StartTask now Text "id"]
      parseCommands' (Arg.Start ["id"] True) `shouldBe` [StartTask now Json "id"]
      parseCommands' (Arg.Start ["id-started"] False) `shouldBe` [Error Text "task already started"]
      parseCommands' (Arg.Start ["id-done"] False) `shouldBe` [Error Text "task already done"]
      parseCommands' (Arg.Start ["id-deleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.Start ["id-unknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse stop command" $ do
      let taskA = Task.new {_id = "id", _active = Just now}
      let taskB = Task.new {_id = "id-stopped"}
      let taskC = Task.new {_id = "id-done", _done = Just now}
      let taskD = Task.new {_id = "id-deleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC, taskD]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.Stop [] False) `shouldBe` []
      parseCommands' (Arg.Stop ["id"] False) `shouldBe` [StopTask now Text "id"]
      parseCommands' (Arg.Stop ["id"] True) `shouldBe` [StopTask now Json "id"]
      parseCommands' (Arg.Stop ["id-stopped"] False) `shouldBe` [Error Text "task already stopped"]
      parseCommands' (Arg.Stop ["id-done"] False) `shouldBe` [Error Text "task already done"]
      parseCommands' (Arg.Stop ["id-deleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.Stop ["id-unknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse do command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "id-done", _done = Just now}
      let taskC = Task.new {_id = "id-deleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.Do [] False) `shouldBe` []
      parseCommands' (Arg.Do ["id"] False) `shouldBe` [DoTask now Text "id"]
      parseCommands' (Arg.Do ["id"] True) `shouldBe` [DoTask now Json "id"]
      parseCommands' (Arg.Do ["id-done"] False) `shouldBe` [Error Text "task already done"]
      parseCommands' (Arg.Do ["id-deleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.Do ["id-unknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse undo command" $ do
      let taskA = Task.new {_id = "id", _done = Just now}
      let taskB = Task.new {_id = "id-undone"}
      let taskC = Task.new {_id = "id-deleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.Undo [] False) `shouldBe` []
      parseCommands' (Arg.Undo ["id"] False) `shouldBe` [UndoTask now Text "id"]
      parseCommands' (Arg.Undo ["id"] True) `shouldBe` [UndoTask now Json "id"]
      parseCommands' (Arg.Undo ["id-undone"] False) `shouldBe` [Error Text "task not yet done"]
      parseCommands' (Arg.Undo ["id-deleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.Undo ["id-unknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse delete command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "id-deleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.Delete [] False) `shouldBe` []
      parseCommands' (Arg.Delete ["id"] False) `shouldBe` [DeleteTask now Text "id"]
      parseCommands' (Arg.Delete ["id"] True) `shouldBe` [DeleteTask now Json "id"]
      parseCommands' (Arg.Delete ["id-deleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.Delete ["id-unknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse undelete command" $ do
      let taskA = Task.new {_id = "id", _deleted = Just now}
      let taskB = Task.new {_id = "id-undeleted"}
      let state = State.new {_tasks = [taskA, taskB]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.Undelete [] False) `shouldBe` []
      parseCommands' (Arg.Undelete ["id"] False) `shouldBe` [UndeleteTask now Text "id"]
      parseCommands' (Arg.Undelete ["id"] True) `shouldBe` [UndeleteTask now Json "id"]
      parseCommands' (Arg.Undelete ["id-undeleted"] False) `shouldBe` [Error Text "task not yet deleted"]
      parseCommands' (Arg.Undelete ["id-unknown"] False) `shouldBe` [Error Text "task not found"]

  it "execute" $ do
    execute State.new (AddTask now Text "id" "desc" Nothing Nothing) `shouldBe` [TaskAdded now "id" "desc" Nothing Nothing]
    execute State.new (AddTask now Text "id" "desc" (Just "proj") Nothing) `shouldBe` [TaskAdded now "id" "desc" (Just "proj") Nothing]
    execute State.new (EditTask now Text "id" "desc" Nothing (Just now)) `shouldBe` [TaskEdited now "id" "desc" Nothing (Just now)]
    execute State.new (StartTask now Text "id") `shouldBe` [TaskStarted now "id"]
    execute State.new (StopTask now Text "id") `shouldBe` [TaskStopped now "id"]
    execute State.new (DoTask now Text "id") `shouldBe` [TaskDid now "id"]
    execute State.new (UndoTask now Text "id") `shouldBe` [TaskUndid now "id"]
    execute State.new (DeleteTask now Text "id") `shouldBe` [TaskDeleted now "id"]
    execute State.new (UndeleteTask now Text "id") `shouldBe` [TaskUndeleted now "id"]
    execute State.new (EditContext now Text Nothing) `shouldBe` [ContextEdited now Nothing]
    execute State.new (EditContext now Text (Just "proj")) `shouldBe` [ContextEdited now (Just "proj")]
    execute State.new (Error Text "msg") `shouldBe` []
