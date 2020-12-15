module CommandSpec (spec) where

import Data.Time (TimeZone, UTCTime)
import Test.Hspec
import qualified Unfog.ArgParser as Arg
import Unfog.Command
import Unfog.Event.Type
import Unfog.Response (ResponseType (..))
import Unfog.State (State (..))
import qualified Unfog.State as State
import Unfog.Task (Task (..))
import qualified Unfog.Task as Task
import Prelude hiding (min)

spec :: Spec
spec = parallel $ do
  let now = read "2020-01-01 00:00:00 UTC" :: UTCTime
  let tzone = read "UTC" :: TimeZone

  describe "parse commands" $ do
    it "parse add command" $ do
      let parseCommands' = parseCommands now tzone State.new "id"
      parseCommands' (Arg.AddTask "desc" Nothing Nothing False) `shouldBe` [AddTask now Text "id" "desc" Nothing Nothing]
      parseCommands' (Arg.AddTask "desc" Nothing Nothing True) `shouldBe` [AddTask now Json "id" "desc" Nothing Nothing]
      parseCommands' (Arg.AddTask "desc" (Just "proj") Nothing False) `shouldBe` [AddTask now Text "id" "desc" (Just "proj") Nothing]
      parseCommands' (Arg.AddTask "desc" Nothing (Just now) False) `shouldBe` [AddTask now Text "id" "desc" Nothing (Just now)]
      parseCommands' (Arg.AddTask "" Nothing Nothing False) `shouldBe` [Error Text "desc missing"]

    it "parse edit command" $ do
      let task = Task.new {_id = "id"}
      let state = State.new {_tasks = [task]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.EditTask "id" "desc" Nothing Nothing False) `shouldBe` [EditTask now Text "id" "desc" Nothing Nothing]
      parseCommands' (Arg.EditTask "id" "desc" Nothing Nothing True) `shouldBe` [EditTask now Json "id" "desc" Nothing Nothing]
      parseCommands' (Arg.EditTask "id" "desc" (Just "proj") Nothing False) `shouldBe` [EditTask now Text "id" "desc" (Just "proj") Nothing]
      parseCommands' (Arg.EditTask "id" "desc" Nothing (Just now) False) `shouldBe` [EditTask now Text "id" "desc" Nothing (Just now)]
      parseCommands' (Arg.EditTask "id" "desc" Nothing (Just now) False) `shouldBe` [EditTask now Text "id" "desc" Nothing (Just now)]
      parseCommands' (Arg.EditTask "idunknown" "desc" Nothing Nothing False) `shouldBe` [Error Text "task not found"]

    it "parse start command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "idstarted", _active = Just now}
      let taskC = Task.new {_id = "iddone", _done = Just now}
      let taskD = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC, taskD]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.StartTask [] False) `shouldBe` []
      parseCommands' (Arg.StartTask ["id"] False) `shouldBe` [StartTask now Text "id"]
      parseCommands' (Arg.StartTask ["id"] True) `shouldBe` [StartTask now Json "id"]
      parseCommands' (Arg.StartTask ["idstarted"] False) `shouldBe` [Error Text "task already started"]
      parseCommands' (Arg.StartTask ["iddone"] False) `shouldBe` [Error Text "task already done"]
      parseCommands' (Arg.StartTask ["iddeleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.StartTask ["idunknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse stop command" $ do
      let taskA = Task.new {_id = "id", _active = Just now}
      let taskB = Task.new {_id = "idstopped"}
      let taskC = Task.new {_id = "iddone", _done = Just now}
      let taskD = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC, taskD]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.StopTask [] False) `shouldBe` []
      parseCommands' (Arg.StopTask ["id"] False) `shouldBe` [StopTask now Text "id"]
      parseCommands' (Arg.StopTask ["id"] True) `shouldBe` [StopTask now Json "id"]
      parseCommands' (Arg.StopTask ["idstopped"] False) `shouldBe` [Error Text "task already stopped"]
      parseCommands' (Arg.StopTask ["iddone"] False) `shouldBe` [Error Text "task already done"]
      parseCommands' (Arg.StopTask ["iddeleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.StopTask ["idunknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse do command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "iddone", _done = Just now}
      let taskC = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.DoTask [] False) `shouldBe` []
      parseCommands' (Arg.DoTask ["id"] False) `shouldBe` [DoTask now Text "id"]
      parseCommands' (Arg.DoTask ["id"] True) `shouldBe` [DoTask now Json "id"]
      parseCommands' (Arg.DoTask ["iddone"] False) `shouldBe` [Error Text "task already done"]
      parseCommands' (Arg.DoTask ["iddeleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.DoTask ["idunknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse undo command" $ do
      let taskA = Task.new {_id = "id", _done = Just now}
      let taskB = Task.new {_id = "idundone"}
      let taskC = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.UndoTask [] False) `shouldBe` []
      parseCommands' (Arg.UndoTask ["id"] False) `shouldBe` [UndoTask now Text "id"]
      parseCommands' (Arg.UndoTask ["id"] True) `shouldBe` [UndoTask now Json "id"]
      parseCommands' (Arg.UndoTask ["idundone"] False) `shouldBe` [Error Text "task not yet done"]
      parseCommands' (Arg.UndoTask ["iddeleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.UndoTask ["idunknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse delete command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.DeleteTask [] False) `shouldBe` []
      parseCommands' (Arg.DeleteTask ["id"] False) `shouldBe` [DeleteTask now Text "id"]
      parseCommands' (Arg.DeleteTask ["id"] True) `shouldBe` [DeleteTask now Json "id"]
      parseCommands' (Arg.DeleteTask ["iddeleted"] False) `shouldBe` [Error Text "task already deleted"]
      parseCommands' (Arg.DeleteTask ["idunknown"] False) `shouldBe` [Error Text "task not found"]

    it "parse undelete command" $ do
      let taskA = Task.new {_id = "id", _deleted = Just now}
      let taskB = Task.new {_id = "idundeleted"}
      let state = State.new {_tasks = [taskA, taskB]}
      let parseCommands' = parseCommands now tzone state ""
      parseCommands' (Arg.UndeleteTask [] False) `shouldBe` []
      parseCommands' (Arg.UndeleteTask ["id"] False) `shouldBe` [UndeleteTask now Text "id"]
      parseCommands' (Arg.UndeleteTask ["id"] True) `shouldBe` [UndeleteTask now Json "id"]
      parseCommands' (Arg.UndeleteTask ["idundeleted"] False) `shouldBe` [Error Text "task not yet deleted"]
      parseCommands' (Arg.UndeleteTask ["idunknown"] False) `shouldBe` [Error Text "task not found"]

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
