module Unfog.Procedure where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (localTimeOfDay, todSec, utc, utcToLocalTime)
import System.Process (system)
import Text.Printf (printf)
import Unfog.ArgOptions
import qualified Unfog.ArgParser as Arg
import qualified Unfog.Config as Config
import Unfog.Duration (showApproxTimeDiffRel)
import qualified Unfog.Process as Process
import Unfog.Response
import qualified Unfog.State as State
import qualified Unfog.Store as Store
import Unfog.Task

data Procedure
  = ShowVersion JsonOpt
  | DoUpgrade
  | ClearCache
  | WatchDueDates
  deriving (Show, Read)

handle :: Arg.Procedure -> IO ()
handle (Arg.ShowVersion jsonOpt) = send (parseResponseType jsonOpt) (VersionResponse "1.0.3")
handle Arg.Upgrade = void $ system "curl -sSL https://raw.githubusercontent.com/soywod/unfog/master/bin/install.sh | bash"
handle Arg.ClearCache = State.clearCache >> putStrLn "Cache cleared!"
handle Arg.WatchDueDates = watchDueDates

-- handle Arg.WatchDueDates = void $ forkIO watchDueDates

watchDueDates :: IO ()
watchDueDates = do
  -- forkIO checkDueDates
  nowSecs <- todSec . localTimeOfDay . utcToLocalTime utc <$> getCurrentTime
  let nextMinInSecs = (+) 1 $ truncate $ realToFrac $ (*) 1000000 $ 60 - nowSecs
  print nextMinInSecs

-- threadDelay nextMinInSecs
-- watchDueDates

checkDueDates :: [Int] -> IO ()
checkDueDates reminders = do
  now <- getCurrentTime
  state <- State.readCache <|> (State.rebuild <$> Store.readFile) <|> return State.new
  reminderCmds <- Config.getReminderCmds <$> Config.readFile
  let tasks = filterWith [notDone, notDeleted, matchingReminders now reminders] $ State.getTasks state
  -- let cmds = foldl1 (\acc cmd -> acc ++ mapM_ (\task -> acc ++ taskToReminderStr now task cmd) tasks) reminderCmds
  -- TODO: setup matrix cmds*tasks
  -- let cmds = map (taskToReminderStr now) tasks
  -- mapM_ Process.spawnSilent . Config.getReminderCmds =<< Config.readFile
  return ()

-- reduceTasks :: UTCTime -> String -> [String] -> Task -> [String]
-- reduceTasks now cmd cmds task = cmds ++ [taskToReminderStr now task cmd]

taskToReminderStr :: UTCTime -> Task -> String -> String
taskToReminderStr now task cmd = printf cmd due desc
  where
    due = showApproxTimeDiffRel now (getDue task)
    desc = getDesc task
