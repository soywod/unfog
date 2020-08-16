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
handle (Arg.ShowVersion jsonOpt) = send (parseResponseType jsonOpt) (VersionResponse "1.0.1")
handle (Arg.Upgrade) = system "curl -sSL https://raw.githubusercontent.com/soywod/unfog/master/bin/install.sh | bash" >> return ()
