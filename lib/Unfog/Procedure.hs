module Unfog.Procedure where

import Control.Monad (void)
import System.Process (system)
import Unfog.Arg.Types
import qualified Unfog.Arg.Types as Arg
import Unfog.Response
import qualified Unfog.State as State

data Procedure
  = ShowVersion JsonOpt
  | DoUpgrade
  | ClearCache
  deriving (Show, Read)

handle :: Arg.Procedure -> IO ()
handle (Arg.ShowVersion jsonOpt) = send (parseResponseType jsonOpt) (VersionResponse "1.0.3")
handle Arg.Upgrade = void $ system "curl -sSL https://raw.githubusercontent.com/soywod/unfog/master/install.sh | bash"
handle Arg.ClearCache = State.clearCache >> putStrLn "Cache cleared!"
