module Unfog.Procedure where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (localTimeOfDay, todSec, utc, utcToLocalTime)
import System.Process (system)
import Text.Printf (printf)
import Unfog.ArgOptions
import qualified Unfog.ArgParser as Arg
import qualified Unfog.Config as Config
import Unfog.Duration (showApproxTimeDiffRel)
import Unfog.Response
import qualified Unfog.State as State
import qualified Unfog.Store as Store
import Unfog.Task

data Procedure
  = ShowVersion JsonOpt
  | DoUpgrade
  | ClearCache
  deriving (Show, Read)

handle :: Arg.Procedure -> IO ()
handle (Arg.ShowVersion jsonOpt) = send (parseResponseType jsonOpt) (VersionResponse "1.0.3")
handle Arg.Upgrade = void $ system "curl -sSL https://raw.githubusercontent.com/soywod/unfog/master/bin/install.sh | bash"
handle Arg.ClearCache = State.clearCache >> putStrLn "Cache cleared!"
