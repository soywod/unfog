module CommandSpec (spec) where

import Data.Time (TimeZone, UTCTime)
import Test.Hspec
import qualified Unfog.Arg.Types as Arg
import Unfog.Command.Handler
import qualified Unfog.Command.Types as Command
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
      let commandsOfArg' = commandsOfArg now tzone State.new "id"
      commandsOfArg' (Arg.AddTask "desc" Nothing Nothing False) `shouldBe` [Command.AddTask now Text "id" "desc" Nothing Nothing]
      commandsOfArg' (Arg.AddTask "desc" Nothing Nothing True) `shouldBe` [Command.AddTask now Json "id" "desc" Nothing Nothing]
      commandsOfArg' (Arg.AddTask "desc" (Just "proj") Nothing False) `shouldBe` [Command.AddTask now Text "id" "desc" (Just "proj") Nothing]
      commandsOfArg' (Arg.AddTask "desc" Nothing (Just now) False) `shouldBe` [Command.AddTask now Text "id" "desc" Nothing (Just now)]
      commandsOfArg' (Arg.AddTask "" Nothing Nothing False) `shouldBe` [Command.Error Text "desc missing"]

    it "parse edit command" $ do
      let task = Task.new {_id = "id"}
      let state = State.new {_tasks = [task]}
      let commandsOfArg' = commandsOfArg now tzone state ""
      commandsOfArg' (Arg.EditTask "id" "desc" Nothing Nothing False) `shouldBe` [Command.EditTask now Text "id" "desc" Nothing Nothing]
      commandsOfArg' (Arg.EditTask "id" "desc" Nothing Nothing True) `shouldBe` [Command.EditTask now Json "id" "desc" Nothing Nothing]
      commandsOfArg' (Arg.EditTask "id" "desc" (Just "proj") Nothing False) `shouldBe` [Command.EditTask now Text "id" "desc" (Just "proj") Nothing]
      commandsOfArg' (Arg.EditTask "id" "desc" Nothing (Just now) False) `shouldBe` [Command.EditTask now Text "id" "desc" Nothing (Just now)]
      commandsOfArg' (Arg.EditTask "id" "desc" Nothing (Just now) False) `shouldBe` [Command.EditTask now Text "id" "desc" Nothing (Just now)]
      commandsOfArg' (Arg.EditTask "idunknown" "desc" Nothing Nothing False) `shouldBe` [Command.Error Text "task not found"]

    it "parse start command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "idstarted", _active = Just now}
      let taskC = Task.new {_id = "iddone", _done = Just now}
      let taskD = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC, taskD]}
      let commandsOfArg' = commandsOfArg now tzone state ""
      commandsOfArg' (Arg.StartTask [] False) `shouldBe` []
      commandsOfArg' (Arg.StartTask ["id"] False) `shouldBe` [Command.StartTask now Text "id"]
      commandsOfArg' (Arg.StartTask ["id"] True) `shouldBe` [Command.StartTask now Json "id"]
      commandsOfArg' (Arg.StartTask ["idstarted"] False) `shouldBe` [Command.Error Text "task already started"]
      commandsOfArg' (Arg.StartTask ["iddone"] False) `shouldBe` [Command.Error Text "task already done"]
      commandsOfArg' (Arg.StartTask ["iddeleted"] False) `shouldBe` [Command.Error Text "task already deleted"]
      commandsOfArg' (Arg.StartTask ["idunknown"] False) `shouldBe` [Command.Error Text "task not found"]

    it "parse stop command" $ do
      let taskA = Task.new {_id = "id", _active = Just now}
      let taskB = Task.new {_id = "idstopped"}
      let taskC = Task.new {_id = "iddone", _done = Just now}
      let taskD = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC, taskD]}
      let commandsOfArg' = commandsOfArg now tzone state ""
      commandsOfArg' (Arg.StopTask [] False) `shouldBe` []
      commandsOfArg' (Arg.StopTask ["id"] False) `shouldBe` [Command.StopTask now Text "id"]
      commandsOfArg' (Arg.StopTask ["id"] True) `shouldBe` [Command.StopTask now Json "id"]
      commandsOfArg' (Arg.StopTask ["idstopped"] False) `shouldBe` [Command.Error Text "task already stopped"]
      commandsOfArg' (Arg.StopTask ["iddone"] False) `shouldBe` [Command.Error Text "task already done"]
      commandsOfArg' (Arg.StopTask ["iddeleted"] False) `shouldBe` [Command.Error Text "task already deleted"]
      commandsOfArg' (Arg.StopTask ["idunknown"] False) `shouldBe` [Command.Error Text "task not found"]

    it "parse do command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "iddone", _done = Just now}
      let taskC = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC]}
      let commandsOfArg' = commandsOfArg now tzone state ""
      commandsOfArg' (Arg.DoTask [] False) `shouldBe` []
      commandsOfArg' (Arg.DoTask ["id"] False) `shouldBe` [Command.DoTask now Text "id"]
      commandsOfArg' (Arg.DoTask ["id"] True) `shouldBe` [Command.DoTask now Json "id"]
      commandsOfArg' (Arg.DoTask ["iddone"] False) `shouldBe` [Command.Error Text "task already done"]
      commandsOfArg' (Arg.DoTask ["iddeleted"] False) `shouldBe` [Command.Error Text "task already deleted"]
      commandsOfArg' (Arg.DoTask ["idunknown"] False) `shouldBe` [Command.Error Text "task not found"]

    it "parse undo command" $ do
      let taskA = Task.new {_id = "id", _done = Just now}
      let taskB = Task.new {_id = "idundone"}
      let taskC = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB, taskC]}
      let commandsOfArg' = commandsOfArg now tzone state ""
      commandsOfArg' (Arg.UndoTask [] False) `shouldBe` []
      commandsOfArg' (Arg.UndoTask ["id"] False) `shouldBe` [Command.UndoTask now Text "id"]
      commandsOfArg' (Arg.UndoTask ["id"] True) `shouldBe` [Command.UndoTask now Json "id"]
      commandsOfArg' (Arg.UndoTask ["idundone"] False) `shouldBe` [Command.Error Text "task not yet done"]
      commandsOfArg' (Arg.UndoTask ["iddeleted"] False) `shouldBe` [Command.Error Text "task already deleted"]
      commandsOfArg' (Arg.UndoTask ["idunknown"] False) `shouldBe` [Command.Error Text "task not found"]

    it "parse delete command" $ do
      let taskA = Task.new {_id = "id"}
      let taskB = Task.new {_id = "iddeleted", _deleted = Just now}
      let state = State.new {_tasks = [taskA, taskB]}
      let commandsOfArg' = commandsOfArg now tzone state ""
      commandsOfArg' (Arg.DeleteTask [] False) `shouldBe` []
      commandsOfArg' (Arg.DeleteTask ["id"] False) `shouldBe` [Command.DeleteTask now Text "id"]
      commandsOfArg' (Arg.DeleteTask ["id"] True) `shouldBe` [Command.DeleteTask now Json "id"]
      commandsOfArg' (Arg.DeleteTask ["iddeleted"] False) `shouldBe` [Command.Error Text "task already deleted"]
      commandsOfArg' (Arg.DeleteTask ["idunknown"] False) `shouldBe` [Command.Error Text "task not found"]

    it "parse undelete command" $ do
      let taskA = Task.new {_id = "id", _deleted = Just now}
      let taskB = Task.new {_id = "idundeleted"}
      let state = State.new {_tasks = [taskA, taskB]}
      let commandsOfArg' = commandsOfArg now tzone state ""
      commandsOfArg' (Arg.UndeleteTask [] False) `shouldBe` []
      commandsOfArg' (Arg.UndeleteTask ["id"] False) `shouldBe` [Command.UndeleteTask now Text "id"]
      commandsOfArg' (Arg.UndeleteTask ["id"] True) `shouldBe` [Command.UndeleteTask now Json "id"]
      commandsOfArg' (Arg.UndeleteTask ["idundeleted"] False) `shouldBe` [Command.Error Text "task not yet deleted"]
      commandsOfArg' (Arg.UndeleteTask ["idunknown"] False) `shouldBe` [Command.Error Text "task not found"]

  it "execute" $ do
    execute State.new (Command.AddTask now Text "id" "desc" Nothing Nothing) `shouldBe` [TaskAdded now "id" "desc" Nothing Nothing]
    execute State.new (Command.AddTask now Text "id" "desc" (Just "proj") Nothing) `shouldBe` [TaskAdded now "id" "desc" (Just "proj") Nothing]
    execute State.new (Command.EditTask now Text "id" "desc" Nothing (Just now)) `shouldBe` [TaskEdited now "id" "desc" Nothing (Just now)]
    execute State.new (Command.StartTask now Text "id") `shouldBe` [TaskStarted now "id"]
    execute State.new (Command.StopTask now Text "id") `shouldBe` [TaskStopped now "id"]
    execute State.new (Command.DoTask now Text "id") `shouldBe` [TaskDid now "id"]
    execute State.new (Command.UndoTask now Text "id") `shouldBe` [TaskUndid now "id"]
    execute State.new (Command.DeleteTask now Text "id") `shouldBe` [TaskDeleted now "id"]
    execute State.new (Command.UndeleteTask now Text "id") `shouldBe` [TaskUndeleted now "id"]
    execute State.new (Command.EditContext now Text Nothing) `shouldBe` [ContextEdited now Nothing]
    execute State.new (Command.EditContext now Text (Just "proj")) `shouldBe` [ContextEdited now (Just "proj")]
    execute State.new (Command.Error Text "msg") `shouldBe` []
