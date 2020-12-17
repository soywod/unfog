module Unfog.Process where

import System.Posix.Types (CPid)
import System.Process (StdStream (NoStream), createProcess, getPid, shell, std_err, std_in, std_out)

spawnSilent :: String -> IO (Maybe CPid)
spawnSilent cmd = do
  (_, _, _, handler) <- createProcess (shell cmd) {std_in = NoStream, std_out = NoStream, std_err = NoStream}
  getPid handler
