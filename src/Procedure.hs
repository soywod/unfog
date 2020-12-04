module Procedure where

import ArgOptions
import qualified ArgParser as Arg
import Control.Monad (void)
import Response
import qualified State
import System.Process (system)

data Procedure
  = ShowVersion JsonOpt
  | DoUpgrade
  | ClearCache
  deriving (Show, Read)

handle :: Arg.Procedure -> IO ()
handle (Arg.ShowVersion jsonOpt) = send (parseResponseType jsonOpt) (VersionResponse "1.0.3")
handle Arg.Upgrade = void $ system "curl -sSL https://raw.githubusercontent.com/soywod/unfog/master/bin/install.sh | bash"
handle Arg.ClearCache = State.clearCache >> putStrLn "Cache cleared!"
