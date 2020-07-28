module ArgParser where

import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import Task
import qualified Parsec

type OnlyTagsOpt = Bool

type OnlyIdsOpt = Bool

type MoreOpt = Bool

type JsonOpt = Bool

data Arg
  = List OnlyIdsOpt OnlyTagsOpt MoreOpt JsonOpt
  | Info Id JsonOpt
  deriving (Show)

-- Options

moreOpt :: String -> Parser MoreOpt
moreOpt h = switch $ long "more" <> help h

jsonOpt :: Parser MoreOpt
jsonOpt = switch $ long "json" <> help "Show result as JSON string"

onlyIdsOpt :: Parser OnlyIdsOpt
onlyIdsOpt = switch $ long "only-ids" <> help "Show only tasks id"

onlyTagsOpt :: Parser OnlyTagsOpt
onlyTagsOpt = switch $ long "only-tags" <> help "Show only tasks tags"

-- Queries

listQuery :: Mod CommandFields Arg
listQuery = command "list" (info parser infoMod)
  where
    parser = List <$> onlyIdsOpt <*> onlyTagsOpt <*> moreOpt "Show more details about tasks" <*> jsonOpt
    infoMod = progDesc "Show tasks filtered by current context"

infoQuery :: Mod CommandFields Arg
infoQuery = command "info" (info parser infoMod)
  where
    parser = Info <$> argument auto (metavar "ID") <*> jsonOpt
    infoMod = progDesc "Show task details"

-- Parser

parseArgs :: IO Arg
parseArgs =
  execParser $ info (opts <**> helper) desc
  where
    desc = fullDesc <> header "⏱ Unfog - Minimalist task & time manager"
    opts = hsubparser $ listQuery <> infoQuery

mapToParsecArg :: Arg -> Parsec.Arg
mapToParsecArg (List moreOpt onlyIdsOpt onlyTagsOpt jsonOpt) =
  Parsec.emptyArgTree
    { Parsec._opts =
        Parsec.ArgOpts
          { Parsec._onlyIds = onlyIdsOpt,
            Parsec._onlyTags = onlyTagsOpt,
            Parsec._more = moreOpt,
            Parsec._json = jsonOpt
          }
    }
mapToParsecArg (Info id jsonOpt) =
  Parsec.emptyArgTree
    { Parsec._cmd = "info",
      Parsec._ids = [id],
      Parsec._opts =
        Parsec.ArgOpts
          { Parsec._onlyIds = False,
            Parsec._onlyTags = False,
            Parsec._more = False,
            Parsec._json = jsonOpt
          }
    }
