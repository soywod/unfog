module Arg.Parser where

import Arg.Add (AddOpts (AddOpts))
import Arg.Context (CtxOpts (CtxOpts))
import Arg.Delete (DeleteOpts (DeleteOpts))
import Arg.Do (DoOpts (DoOpts))
import Arg.Edit (EditOpts (EditOpts))
import Arg.Info (InfoOpts (InfoOpts))
import Arg.List (ListOpts (ListOpts))
import Arg.Start (StartOpts (StartOpts))
import Arg.Status (StatusOpts (StatusOpts))
import Arg.Stop (StopOpts (StopOpts))
import Arg.Undo (UndoOpts (UndoOpts))
import Arg.Worktime (WtimeOpts (WtimeOpts))
import Control.Monad
import Data.Fixed
import Data.List
import Data.Semigroup ((<>))
import Data.Time
import Options.Applicative
import Text.Read
import Task (Id, Tag)

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
  | Edit EditOpts
  | Start StartOpts
  | Stop StopOpts
  | Do DoOpts
  | Undo UndoOpts
  | Delete DeleteOpts
  | Ctx CtxOpts
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
  foldr1
    (<>)
    [ listQuery,
      infoQuery,
      wtimeQuery now tzone,
      statusQuery,
      upgradeQuery,
      versionQuery
    ]

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
    opts = InfoOpts <$> idParser <*> jsonOptParser

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
commands now tzone =
  foldr1
    (<>)
    [ addCommand,
      editCommand,
      startCommand,
      stopCommand,
      doCommand,
      undoCommand,
      deleteCommand,
      ctxCommand
    ]

addCommand :: Mod CommandFields Arg
addCommand = command "add" (info parser infoMod)
  where
    infoMod = progDesc "Add a new task"
    parser = Add <$> AddOpts <$> descParser

editCommand :: Mod CommandFields Arg
editCommand = command "edit" (info parser infoMod)
  where
    infoMod = progDesc "Edit an existing task"
    parser = Edit <$> (EditOpts <$> idParser <*> descParser)

startCommand :: Mod CommandFields Arg
startCommand = command "start" (info parser infoMod)
  where
    infoMod = progDesc "Start a task"
    parser = Start <$> StartOpts <$> idsParser

stopCommand :: Mod CommandFields Arg
stopCommand = command "stop" (info parser infoMod)
  where
    infoMod = progDesc "Stop a task"
    parser = Stop <$> StopOpts <$> idsParser

doCommand :: Mod CommandFields Arg
doCommand = command "do" (info parser infoMod)
  where
    infoMod = progDesc "Mark as done a task"
    parser = Do <$> DoOpts <$> idsParser

undoCommand :: Mod CommandFields Arg
undoCommand = command "undo" (info parser infoMod)
  where
    infoMod = progDesc "Unmark as done a task"
    parser = Undo <$> UndoOpts <$> idsParser

deleteCommand :: Mod CommandFields Arg
deleteCommand = command "delete" (info parser infoMod)
  where
    infoMod = progDesc "Delete a task"
    parser = Delete <$> DeleteOpts <$> idsParser

ctxCommand :: Mod CommandFields Arg
ctxCommand = command "context" (info parser infoMod)
  where
    infoMod = progDesc "Change the current context"
    parser = Ctx <$> CtxOpts <$> tagsParser

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

-- Parsers

idParser :: Parser Id
idParser = argument auto (metavar "ID")

idsParser :: Parser [Id]
idsParser = some idParser

descParser :: Parser String
descParser = unwords <$> some (argument str (metavar "DESC"))

tagsParser :: Parser [Tag]
tagsParser = many (argument str (metavar "TAGS..."))

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
