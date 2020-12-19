module Unfog.Subscriber.Notifier where

import Unfog.Command.Types (Command)
import qualified Unfog.Subscriber.Logger as Logger (subscriber)
import Unfog.Subscriber.Types (ShortenId)

notify :: ShortenId -> [Command] -> IO ()
notify shortenId cmds = mapM_ (notify' cmds) subscribers
  where
    subscribers = [Logger.subscriber]
    notify' cmds subscriber = mapM_ (subscriber shortenId) cmds
