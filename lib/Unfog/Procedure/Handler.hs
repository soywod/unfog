module Unfog.Procedure.Handler where

import Control.Monad (void)
import System.Process (system)
import qualified Unfog.Arg.Types as Arg
import Unfog.Response
import qualified Unfog.State as State

showVersion :: Arg.JsonOpt -> IO ()
showVersion jsonOpt = send rtype $ VersionResponse "1.0.6"
  where
    rtype = parseResponseType jsonOpt

upgrade :: IO ()
upgrade = void $ system "curl -sSL https://raw.githubusercontent.com/soywod/unfog/master/install.sh | bash"

clearCache :: IO ()
clearCache = do
  State.clearCache
  putStrLn "Cache cleared!"

handle :: Arg.Procedure -> IO ()
handle (Arg.ShowVersion jsonOpt) = showVersion jsonOpt
handle Arg.Upgrade = upgrade
handle Arg.ClearCache = clearCache
