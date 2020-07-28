module Arg.Parser where

import Arg.Info (InfoOpts (InfoOpts))
import Arg.List (ListOpts (ListOpts))
import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative

type OnlyTagsOpt = Bool

type OnlyIdsOpt = Bool

type MoreOpt = Bool

type JsonOpt = Bool

data Arg
  = List ListOpts
  | Info InfoOpts
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
listQuery = command "list" $ info parser infoMod
  where
    parser = List <$> opts
    opts = ListOpts <$> onlyIdsOpt <*> onlyTagsOpt <*> moreOpt "Show more details about tasks" <*> jsonOpt
    infoMod = progDesc "Show tasks filtered by current context"

infoQuery :: Mod CommandFields Arg
infoQuery = command "info" $ info parser infoMod
  where
    parser = Info <$> opts
    opts = InfoOpts <$> argument auto (metavar "ID") <*> jsonOpt
    infoMod = progDesc "Show task details"

-- Commands

-- Parser

parseArgs :: IO Arg
parseArgs =
  execParser $ info (opts <**> helper) desc
  where
    desc = fullDesc <> header "⏱ Unfog - Minimalist task & time manager"
    opts = hsubparser $ listQuery <> infoQuery
