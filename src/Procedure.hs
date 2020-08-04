module Procedure where

import ArgOptions
import qualified ArgParser as Arg
import Response
import System.Process (system)

data Procedure
  = ShowVersion JsonOpt
  | DoUpgrade
  deriving (Show, Read)

handle :: Arg.Procedure -> IO ()
handle (Arg.Version jsonOpt) = send (parseResponseType jsonOpt) (VersionResponse "1.0.0")
handle (Arg.Upgrade) = system "curl -sSL https://raw.githubusercontent.com/soywod/unfog.cli/master/bin/install.sh | sh" >> return ()
