module Unfog.Subscriber.Logger where

import Text.Printf (printf)
import Unfog.Command.Types (Command (..))
import Unfog.Response (Response (..), ResponseType (..), send)
import Unfog.Subscriber.Types (Subscriber)

subscriber :: Subscriber
subscriber shortenId cmd = case cmd of
  AddTask _ Text id _ proj _ -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m added%s" (shortenId id) $ showIfJust " to project \x1b[34m%s\x1b[0m" proj
  AddTask _ Json id _ proj _ -> send Json $ MessageResponse $ printf "Task %s added%s" (shortenId id) $ showIfJust " to project %s" proj
  EditTask _ Text id _ _ _ -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m edited" (shortenId id)
  EditTask _ Json id _ _ _ -> send Json $ MessageResponse $ printf "Task %s edited" (shortenId id)
  StartTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m started" (shortenId id)
  StartTask _ Json id -> send Json $ MessageResponse $ printf "Task %s started" (shortenId id)
  StopTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m stopped" (shortenId id)
  StopTask _ Json id -> send Json $ MessageResponse $ printf "Task %s stopped" (shortenId id)
  DoTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m done" (shortenId id)
  DoTask _ Json id -> send Json $ MessageResponse $ printf "Task %s done" (shortenId id)
  UndoTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m undone" (shortenId id)
  UndoTask _ Json id -> send Json $ MessageResponse $ printf "Task %s undone" (shortenId id)
  DeleteTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m deleted" (shortenId id)
  DeleteTask _ Json id -> send Json $ MessageResponse $ printf "Task %s deleted" (shortenId id)
  UndeleteTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m undeleted" (shortenId id)
  UndeleteTask _ Json id -> send Json $ MessageResponse $ printf "Task %s undeleted" (shortenId id)
  EditContext _ rtype Nothing -> send rtype $ MessageResponse "Context cleared"
  EditContext _ Text (Just ctx) -> send Text $ MessageResponse $ printf "Context set [\x1b[34m%s\x1b[0m]" ctx
  EditContext _ Json (Just ctx) -> send Json $ MessageResponse $ printf "Context set [%s]" ctx
  Error rtype msg -> send rtype $ ErrorResponse msg
  where
    showIfJust _ Nothing = ""
    showIfJust msg (Just a) = printf msg a
