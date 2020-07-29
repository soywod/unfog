module Arg.Parser where

import Arg.Info (InfoOpts (InfoOpts))
import Arg.List (ListOpts (ListOpts))
import Arg.Worktime (WtimeOpts (WtimeOpts))
import Arg.Status (StatusOpts (StatusOpts))
import Control.Monad
import Data.Fixed
import Data.Semigroup ((<>))
import Data.Time
import Options.Applicative
import Text.Read

type FromOpt = Maybe UTCTime

type ToOpt = Maybe UTCTime

type OnlyTagsOpt = Bool

type OnlyIdsOpt = Bool

type VersionOpt = Bool

type MoreOpt = Bool

type JsonOpt = Bool

data Arg
  = List ListOpts
  | Info InfoOpts
  | Wtime WtimeOpts
  | Status StatusOpts
  | Upgrade
  | Version
  deriving (Show)

-- Helpers

readUTCTime :: TimeZone -> String -> Maybe UTCTime
readUTCTime tzone = parseLocalTime >=> toUTC
  where
    parseLocalTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
    toUTC = return . localTimeToUTC tzone

-- Options

fromOpt :: UTCTime -> TimeZone -> Parser FromOpt
fromOpt now tzone =
  option (fromDateParser now tzone) $
    long "from"
      <> short 'f'
      <> metavar "DATE"
      <> value Nothing
      <> help "Start date of interval"

toOpt :: UTCTime -> TimeZone -> Parser FromOpt
toOpt now tzone =
  option (fromDateParser now tzone) $
    long "to"
      <> short 't'
      <> metavar "DATE"
      <> value Nothing
      <> help "End date of interval"

onlyIdsOpt :: Parser OnlyIdsOpt
onlyIdsOpt = switch $ long "only-ids" <> help "Show only tasks id"

onlyTagsOpt :: Parser OnlyTagsOpt
onlyTagsOpt = switch $ long "only-tags" <> help "Show only tasks tags"

moreOpt :: String -> Parser MoreOpt
moreOpt h = switch $ long "more" <> help h

jsonOpt :: Parser MoreOpt
jsonOpt = switch $ long "json" <> help "Show result as JSON string"

-- Queries

queries :: UTCTime -> TimeZone -> Mod CommandFields Arg
queries now tzone =
  listQuery
    <> infoQuery
    <> wtimeQuery now tzone
    <> statusQuery
    <> upgradeQuery
    <> versionQuery

listQuery :: Mod CommandFields Arg
listQuery = command "list" $ info parser infoMod
  where
    infoMod = progDesc "Show tasks filtered by current context"
    parser = List <$> opts
    opts = ListOpts <$> onlyIdsOpt <*> onlyTagsOpt <*> moreOpt "Show more details about tasks" <*> jsonOpt

infoQuery :: Mod CommandFields Arg
infoQuery = command "info" $ info parser infoMod
  where
    infoMod = progDesc "Show task details"
    parser = Info <$> opts
    opts = InfoOpts <$> argument auto (metavar "ID") <*> jsonOpt

wtimeQuery :: UTCTime -> TimeZone -> Mod CommandFields Arg
wtimeQuery now tzone = command "worktime" $ info parser infoMod
  where
    infoMod = progDesc "Show task details"
    parser = Wtime <$> opts
    opts =
      WtimeOpts
        <$> many (argument str (metavar "TAGS..."))
        <*> fromOpt now tzone
        <*> toOpt now tzone
        <*> moreOpt "Show more details about worktime"
        <*> jsonOpt

statusQuery :: Mod CommandFields Arg
statusQuery = command "status" $ info parser infoMod
  where
    infoMod = progDesc "Show the total amount of time spent on the current active task"
    parser = Status <$> opts
    opts = StatusOpts <$> moreOpt "Show more details about the task" <*> jsonOpt

upgradeQuery :: Mod CommandFields Arg
upgradeQuery = command "upgrade" $ info parser infoMod
  where
    infoMod = progDesc "Upgrade the CLI"
    parser = pure Upgrade

versionQuery :: Mod CommandFields Arg
versionQuery = command "version" $ info parser infoMod
  where
    infoMod = progDesc "Show the version"
    parser = pure Version

-- Commands

-- Parsers

dateParser :: String -> UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
dateParser timefmt now tzone = eitherReader parseDate
  where
    parseDate str = Right $ parseDate' <|> parseTime' <|> parseDateTime'
      where
        parseDate' = readUTCTime tzone (str ++ " " ++ timefmt)
        parseTime' = readUTCTime tzone (formatTime defaultTimeLocale "%Y-%m-%d " now ++ str)
        parseDateTime' = readUTCTime tzone str

fromDateParser :: UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
fromDateParser = dateParser "00:00"

toDateParser :: UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
toDateParser = dateParser "23:59"

parseArgs :: IO Arg
parseArgs = do
  now <- getCurrentTime
  tzone <- getCurrentTimeZone
  let desc = fullDesc <> header "⏱ Unfog - Minimalist task & time manager"
  let parser = helper <*> hsubparser (queries now tzone)
  execParser $ info parser desc
