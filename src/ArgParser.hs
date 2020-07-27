module ArgParser where

import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import Task
import qualified Parsec

type MoreOpt = Bool

type JsonOpt = Bool

data Arg
  = List MoreOpt JsonOpt
  | Info Id
  deriving (Show)

moreOpt :: String -> Parser MoreOpt
moreOpt h = switch $ long "more" <> help h

jsonOpt :: Parser MoreOpt
jsonOpt = switch $ long "json" <> help "Show result as JSON string"

listCommand :: Mod CommandFields Arg
listCommand = command "list" (info parser infoMod)
  where
    parser = List <$> moreOpt "Show more details about tasks" <*> jsonOpt
    infoMod = progDesc "List tasks filtered by current context"

parseArgs :: IO Arg
parseArgs =
  execParser (info (opts <**> helper) desc)
  where
    opts =
      hsubparser listCommand
    desc = (fullDesc <> header "⏱ Unfog - Minimalist task & time manager")

mapToParsecArg :: Arg -> Parsec.Arg
mapToParsecArg (List moreOpt jsonOpt) =
  Parsec.emptyArgTree
    { Parsec._opts =
        Parsec.ArgOpts
          { Parsec._more = moreOpt,
            Parsec._json = jsonOpt,
            Parsec._onlyIds = False,
            Parsec._onlyTags = False
          }
    }
