module Arg.Parser where

import Arg.Add (AddOpts (AddOpts))
import Arg.Info (InfoOpts (InfoOpts))
import Arg.List (ListOpts (ListOpts))
import Arg.Status (StatusOpts (StatusOpts))
import Arg.Worktime (WtimeOpts (WtimeOpts))
import Control.Monad
import Data.Fixed
import Data.List
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
  | Add AddOpts
  deriving (Show)

parseArgs :: IO Arg
parseArgs = do
  now <- getCurrentTime
  tzone <- getCurrentTimeZone
  let desc = fullDesc <> header "⏱ Unfog - Minimalist task & time manager"
  let queries' = queries now tzone
  let commands' = commands now tzone
  let parser = helper <*> hsubparser (queries' <> commands')
  let prefs' = prefs showHelpOnError
  customExecParser prefs' (info parser desc)

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
    opts = ListOpts <$> onlyIdsOptParser <*> onlyTagsOptParser <*> moreOptParser "Show more details about tasks" <*> jsonOptParser

infoQuery :: Mod CommandFields Arg
infoQuery = command "info" $ info parser infoMod
  where
    infoMod = progDesc "Show task details"
    parser = Info <$> opts
    opts = InfoOpts <$> argument auto (metavar "ID") <*> jsonOptParser

wtimeQuery :: UTCTime -> TimeZone -> Mod CommandFields Arg
wtimeQuery now tzone = command "worktime" $ info parser infoMod
  where
    infoMod = progDesc "Show task details"
    parser = Wtime <$> opts
    opts =
      WtimeOpts
        <$> many (argument str (metavar "TAGS..."))
        <*> fromOptParser now tzone
        <*> toOptParser now tzone
        <*> moreOptParser "Show more details about worktime"
        <*> jsonOptParser

statusQuery :: Mod CommandFields Arg
statusQuery = command "status" $ info parser infoMod
  where
    infoMod = progDesc "Show the total amount of time spent on the current active task"
    parser = Status <$> opts
    opts = StatusOpts <$> moreOptParser "Show more details about the task" <*> jsonOptParser

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

commands :: UTCTime -> TimeZone -> Mod CommandFields Arg
commands now tzone = addCommand

addCommand :: Mod CommandFields Arg
addCommand = command "add" (info parser infoMod)
  where
    infoMod = progDesc "Add a new task"
    descParser = unwords <$> some (argument str (metavar "DESC"))
    parser = Add <$> AddOpts <$> descParser

-- Readers

readUTCTime :: TimeZone -> String -> Maybe UTCTime
readUTCTime tzone = parseLocalTime >=> toUTC
  where
    parseLocalTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
    toUTC = return . localTimeToUTC tzone

dateReader :: String -> UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
dateReader timefmt now tzone = maybeReader parseDate
  where
    parseDate str = Just $ parseDate' <|> parseTime' <|> parseDateTime'
      where
        parseDate' = readUTCTime tzone (str ++ " " ++ timefmt)
        parseTime' = readUTCTime tzone (formatTime defaultTimeLocale "%Y-%m-%d " now ++ str)
        parseDateTime' = readUTCTime tzone str

fromDateReader :: UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
fromDateReader = dateReader "00:00"

toDateReader :: UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
toDateReader = dateReader "23:59"

tagReader :: ReadM String
tagReader = maybeReader parseTag
  where
    parseTag "" = Nothing
    parseTag ('+' : tag) = Just tag
    parseTag _ = Nothing

-- Parsers

fromOptParser :: UTCTime -> TimeZone -> Parser FromOpt
fromOptParser now tzone =
  option (fromDateReader now tzone) $
    long "from"
      <> short 'f'
      <> metavar "DATE"
      <> value Nothing
      <> help "Start date of interval"

toOptParser :: UTCTime -> TimeZone -> Parser FromOpt
toOptParser now tzone =
  option (fromDateReader now tzone) $
    long "to"
      <> short 't'
      <> metavar "DATE"
      <> value Nothing
      <> help "End date of interval"

onlyIdsOptParser :: Parser OnlyIdsOpt
onlyIdsOptParser = switch $ long "only-ids" <> help "Show only tasks id"

onlyTagsOptParser :: Parser OnlyTagsOpt
onlyTagsOptParser = switch $ long "only-tags" <> help "Show only tasks tags"

moreOptParser :: String -> Parser MoreOpt
moreOptParser h = switch $ long "more" <> help h

jsonOptParser :: Parser MoreOpt
jsonOptParser = switch $ long "json" <> help "Show result as JSON string"
