module Unfog.Subscriber.Types where

import Unfog.Command.Types (Command)
import Unfog.Task (Id, ShortId)

type ShortenId = Id -> ShortId

type Subscriber = ShortenId -> Command -> IO ()
