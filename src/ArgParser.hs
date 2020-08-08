module ArgParser where

import ArgOptions
import Control.Monad
import Data.List
import Data.Time
import Event
import Options.Applicative
import Task (Desc, Due, Id, Project)

data Query
  = List OnlyIdsOpt OnlyProjsOpt JsonOpt
  | Info Id JsonOpt
  | Wtime Project FromOpt ToOpt MoreOpt JsonOpt
  | Status MoreOpt JsonOpt
  deriving (Show)

data Command
  = Add Desc ProjOpt DueOpt JsonOpt
  | Edit Id Desc ProjOpt DueOpt JsonOpt
  | Start [Id] JsonOpt
  | Stop [Id] JsonOpt
  | Do [Id] JsonOpt
  | Undo [Id] JsonOpt
  | Delete [Id] JsonOpt
  | Undelete [Id] JsonOpt
  | Context Project JsonOpt
  deriving (Show)

data Procedure
  = Version JsonOpt
  | Upgrade

data Arg = CommandArg Command | QueryArg Query | ProcedureArg Procedure

parseArgs :: IO Arg
parseArgs = do
  now <- getCurrentTime
  tzone <- getCurrentTimeZone
  let desc = fullDesc <> header "⏱ Unfog - Minimalist task & time manager"
  let queries' = queries now tzone
  let commands' = commands now tzone
  let parser = helper <*> hsubparser (queries' <> commands' <> procedures)
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
      statusQuery
    ]

listQuery :: Mod CommandFields Arg
listQuery = aliasedCommand parser infoMod ["list", "l"]
  where
    infoMod = progDesc "Show tasks filtered by current context"
    parser = QueryArg <$> (List <$> onlyIdsOptParser <*> onlyProjsOptParser <*> jsonOptParser)

infoQuery :: Mod CommandFields Arg
infoQuery = aliasedCommand parser infoMod ["info", "i"]
  where
    infoMod = progDesc "Show task details"
    parser = QueryArg <$> (Info <$> idParser <*> jsonOptParser)

wtimeQuery :: UTCTime -> TimeZone -> Mod CommandFields Arg
wtimeQuery now tzone = aliasedCommand parser infoMod ["worktime", "wtime", "w"]
  where
    infoMod = progDesc "Show worktime report"
    parser =
      QueryArg
        <$> ( Wtime
                <$> projParser
                <*> fromOptParser now tzone
                <*> toOptParser now tzone
                <*> moreOptParser "Show more details about worktime"
                <*> jsonOptParser
            )

statusQuery :: Mod CommandFields Arg
statusQuery = command "status" (info parser infoMod)
  where
    infoMod = progDesc "Show time spent on current active task"
    parser = QueryArg <$> (Status <$> moreOptParser "Show more details about the task" <*> jsonOptParser)

-- Commands

commands :: UTCTime -> TimeZone -> Mod CommandFields Arg
commands now tzone =
  foldr1
    (<>)
    [ addCommand now tzone,
      editCommand now tzone,
      startCommand,
      stopCommand,
      doCommand,
      undoCommand,
      deleteCommand,
      ctxCommand
    ]

addCommand :: UTCTime -> TimeZone -> Mod CommandFields Arg
addCommand now tzone = aliasedCommand parser infoMod ["add", "a"]
  where
    infoMod = progDesc "Add a new task"
    parser = CommandArg <$> (Add <$> descParser <*> projOptParser <*> dueOptParser now tzone <*> jsonOptParser)

editCommand :: UTCTime -> TimeZone -> Mod CommandFields Arg
editCommand now tzone = aliasedCommand parser infoMod ["edit", "e"]
  where
    infoMod = progDesc "Edit an existing task"
    parser = CommandArg <$> (Edit <$> idParser <*> descParser <*> projOptParser <*> dueOptParser now tzone <*> jsonOptParser)

startCommand :: Mod CommandFields Arg
startCommand = command "start" (info parser infoMod)
  where
    infoMod = progDesc "Start a task"
    parser = CommandArg <$> (Start <$> idsParser <*> jsonOptParser)

stopCommand :: Mod CommandFields Arg
stopCommand = command "stop" (info parser infoMod)
  where
    infoMod = progDesc "Stop a task"
    parser = CommandArg <$> (Stop <$> idsParser <*> jsonOptParser)

doCommand :: Mod CommandFields Arg
doCommand = aliasedCommand parser infoMod ["done", "do", "d"]
  where
    infoMod = progDesc "Mark as done a task"
    parser = CommandArg <$> (Do <$> idsParser <*> jsonOptParser)

undoCommand :: Mod CommandFields Arg
undoCommand = aliasedCommand parser infoMod ["undone", "undo", "u"]
  where
    infoMod = progDesc "Unmark as done a task"
    parser = CommandArg <$> (Undo <$> idsParser <*> jsonOptParser)

deleteCommand :: Mod CommandFields Arg
deleteCommand = aliasedCommand parser infoMod ["delete", "del", "D"]
  where
    infoMod = progDesc "Delete a task"
    parser = CommandArg <$> (Delete <$> idsParser <*> jsonOptParser)

ctxCommand :: Mod CommandFields Arg
ctxCommand = aliasedCommand parser infoMod ["context", "ctx", "c"]
  where
    infoMod = progDesc "Change the current context"
    parser = CommandArg <$> (Context <$> projParser <*> jsonOptParser)

-- Procedures

procedures :: Mod CommandFields Arg
procedures =
  foldr1
    (<>)
    [ upgradeProcedure,
      versionProcedure
    ]

upgradeProcedure :: Mod CommandFields Arg
upgradeProcedure = command "upgrade" (info parser infoMod)
  where
    infoMod = progDesc "Upgrade the CLI"
    parser = pure $ ProcedureArg Upgrade

versionProcedure :: Mod CommandFields Arg
versionProcedure = command "version" (info parser infoMod)
  where
    infoMod = progDesc "Show the version"
    parser = ProcedureArg <$> Version <$> jsonOptParser

-- Readers

readUTCTime :: TimeZone -> String -> Maybe UTCTime
readUTCTime tzone = toUTC <=< parseLocalTime
  where
    parseLocalTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
    toUTC = return . localTimeToUTC tzone

dateReader :: String -> UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
dateReader timefmt now tzone = eitherReader reader
  where
    reader str = case parseDate str of
      Nothing -> Left $ "invalid date format `" ++ str ++ "' (should match YYYY-MM-DD HH:MM)"
      Just date -> Right $ Just date
    parseDate str = Nothing <|> parseDate' <|> parseTime' <|> parseDateTime'
      where
        parseDate' = readUTCTime tzone $ str ++ " " ++ timefmt
        parseTime' = readUTCTime tzone $ formatTime defaultTimeLocale "%Y-%m-%d " now ++ str
        parseDateTime' = readUTCTime tzone str

dueDateReader :: UTCTime -> TimeZone -> ReadM FromOpt
dueDateReader = dateReader "00:00"

fromDateReader :: UTCTime -> TimeZone -> ReadM FromOpt
fromDateReader = dateReader "00:00"

toDateReader :: UTCTime -> TimeZone -> ReadM ToOpt
toDateReader = dateReader "23:59"

projectReader :: ReadM (Maybe String)
projectReader = maybeReader projectReader'
  where
    projectReader' str
      | null str = pure Nothing
      | otherwise = pure $ Just str

-- Parsers

idParser :: Parser Id
idParser = argument str $ metavar "ID" <> completer idsCompleter

idsParser :: Parser [Id]
idsParser = some idParser

descParser :: Parser Desc
descParser = unwords <$> (many $ argument str $ metavar "DESC")

projParser :: Parser Project
projParser = argument projectReader $ metavar "PROJECT" <> value Nothing <> completer projCompleter

-- instance IsString (Maybe String)

projOptParser :: Parser ProjOpt
projOptParser = readMaybeProj <$> parser
  where
    parser = option str $ long "project" <> short 'p' <> metavar "PROJECT" <> value "" <> help "Override task project"
    readMaybeProj proj
      | null proj = Nothing
      | otherwise = Just proj

dueOptParser :: UTCTime -> TimeZone -> Parser FromOpt
dueOptParser now tzone =
  option (dueDateReader now tzone) $
    long "due"
      <> short 'd'
      <> metavar "DATE"
      <> value Nothing
      <> help "Due date"

fromOptParser :: UTCTime -> TimeZone -> Parser FromOpt
fromOptParser now tzone =
  option (fromDateReader now tzone) $
    long "from"
      <> short 'f'
      <> metavar "DATE"
      <> value Nothing
      <> help "Interval start date"

toOptParser :: UTCTime -> TimeZone -> Parser FromOpt
toOptParser now tzone =
  option (fromDateReader now tzone) $
    long "to"
      <> short 't'
      <> metavar "DATE"
      <> value Nothing
      <> help "Interval end date"

onlyIdsOptParser :: Parser OnlyIdsOpt
onlyIdsOptParser = switch $ long "only-ids" <> help "Show only tasks id"

onlyProjsOptParser :: Parser OnlyProjsOpt
onlyProjsOptParser = switch $ long "only-projects" <> help "Show only tasks projects"

moreOptParser :: String -> Parser MoreOpt
moreOptParser h = switch $ long "more" <> help h

jsonOptParser :: Parser MoreOpt
jsonOptParser = switch $ long "json" <> help "Show result as JSON string"

-- Completers

idsCompleter :: Completer
idsCompleter = mkCompleter idsCompleter'
  where
    idsCompleter' str = sort . filter (isPrefixOf str) . foldl extractIds [] <$> readEvents
    extractIds ids (TaskAdded _ id _ _ _) = ids ++ [id]
    extractIds ids _ = ids

projCompleter :: Completer
projCompleter = mkCompleter projectCompleter'
  where
    projectCompleter' str = sort . nub . filter (isPrefixOf str) . foldl extractProjects [] <$> readEvents
    extractProjects projs (TaskAdded _ _ _ (Just proj) _) = projs ++ [proj]
    extractProjects projs (TaskEdited _ _ _ (Just proj) _) = projs ++ [proj]
    extractProjects projs (ContextEdited (Just proj)) = projs ++ [proj]
    extractProjects projs _ = projs

-- Helpers

aliasedCommand :: Parser Arg -> InfoMod Arg -> [String] -> Mod CommandFields Arg
aliasedCommand parser infoMod (cmd : aliases) = aliases' <> cmd'
  where
    cmd' = command cmd (info parser infoMod)
    aliases' = foldr1 (<>) $ reverse $ map (flip command (info parser idm)) aliases
