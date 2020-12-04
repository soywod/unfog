module ArgParser where

import ArgOptions
import Control.Monad
import Data.List
import Data.Time
import Event.Type (Event (..))
import Options.Applicative
import qualified Store
import qualified System.Environment as Env
import Task (Desc, Id, Project, skipDashes)

data Query
  = ShowTasks DoneOpt DeletedOpt JsonOpt
  | ShowTask Id JsonOpt
  | ShowWorktime Project FromOpt ToOpt MoreOpt JsonOpt
  | ShowStatus MoreOpt JsonOpt
  deriving (Show, Eq)

data Command
  = AddTask Desc ProjOpt DueOpt JsonOpt
  | EditTask Id Desc ProjOpt DueOpt JsonOpt
  | StartTask [Id] JsonOpt
  | StopTask [Id] JsonOpt
  | ToggleTask [Id] JsonOpt
  | DoTask [Id] JsonOpt
  | UndoTask [Id] JsonOpt
  | DeleteTask [Id] JsonOpt
  | UndeleteTask [Id] JsonOpt
  | EditContext Project JsonOpt
  deriving (Show, Eq)

data Procedure
  = ShowVersion JsonOpt
  | Upgrade
  | ClearCache

data Arg
  = CommandArg Command
  | QueryArg Query
  | ProcedureArg Procedure

parseArgs :: IO Arg
parseArgs = parseArgs' =<< Env.getArgs
  where
    parseArgs' args
      | null args = return $ QueryArg $ ShowTasks False False False
      | otherwise = execParser'

execParser' :: IO Arg
execParser' = do
  now <- getCurrentTime
  tzone <- getCurrentTimeZone
  let desc = fullDesc <> header "⏱ Unfog - Minimalist task & time manager"
  let prefs' = prefs showHelpOnError
  let mainQueries' = mainQueries now tzone
  let aliasesQueries' = aliasesQueries now tzone
  let mainCommands' = mainCommands now tzone
  let aliasesCommands' = aliasesCommands now tzone
  let mainParser = helper <*> hsubparser (mainQueries' <> mainCommands' <> procedures)
  let aliasesParser = subparser (aliasesQueries' <> aliasesCommands' <> internal)
  let parser = mainParser <|> aliasesParser
  customExecParser prefs' $ info parser desc

-- Queries

queries :: UTCTime -> TimeZone -> [(Mod CommandFields Arg, Mod CommandFields Arg)]
queries now tzone =
  [ listQueries,
    infoQueries,
    wtimeQueries now tzone,
    statusQueries
  ]

mainQueries :: UTCTime -> TimeZone -> Mod CommandFields Arg
mainQueries now tzone = foldr1 (<>) $ map fst $ queries now tzone

aliasesQueries :: UTCTime -> TimeZone -> Mod CommandFields Arg
aliasesQueries now tzone = foldr1 (<>) $ map snd $ queries now tzone

listQueries :: (Mod CommandFields Arg, Mod CommandFields Arg)
listQueries = aliasedCommand parser desc ["list", "l"]
  where
    desc = "Show current project tasks"
    parser = QueryArg <$> (ShowTasks <$> doneOptParser <*> deletedOptParser <*> jsonOptParser)

infoQueries :: (Mod CommandFields Arg, Mod CommandFields Arg)
infoQueries = aliasedCommand parser desc ["info", "i"]
  where
    desc = "Show task details"
    parser = QueryArg <$> (ShowTask <$> idParser <*> jsonOptParser)

wtimeQueries :: UTCTime -> TimeZone -> (Mod CommandFields Arg, Mod CommandFields Arg)
wtimeQueries now tzone = aliasedCommand parser desc ["worktime", "wtime", "w"]
  where
    desc = "Show worktime report"
    parser =
      QueryArg
        <$> ( ShowWorktime
                <$> projParser
                <*> fromOptParser now tzone
                <*> toOptParser now tzone
                <*> moreOptParser "Show more details about worktime"
                <*> jsonOptParser
            )

statusQueries :: (Mod CommandFields Arg, Mod CommandFields Arg)
statusQueries = aliasedCommand parser desc ["status", "stat"]
  where
    desc = "Show active task info"
    parser = QueryArg <$> (ShowStatus <$> moreOptParser "Show more details about the task" <*> jsonOptParser)

-- Commands

commands :: UTCTime -> TimeZone -> [(Mod CommandFields Arg, Mod CommandFields Arg)]
commands now tzone =
  [ addCommands now tzone,
    editCommands now tzone,
    startCommands,
    stopCommands,
    toggleCommands,
    doCommands,
    undoCommands,
    deleteCommands,
    undeleteCommands,
    ctxCommands
  ]

mainCommands :: UTCTime -> TimeZone -> Mod CommandFields Arg
mainCommands now tzone = foldr1 (<>) $ map fst $ commands now tzone

aliasesCommands :: UTCTime -> TimeZone -> Mod CommandFields Arg
aliasesCommands now tzone = foldr1 (<>) $ map snd $ commands now tzone

addCommands :: UTCTime -> TimeZone -> (Mod CommandFields Arg, Mod CommandFields Arg)
addCommands now tzone = aliasedCommand parser desc ["add", "a"]
  where
    desc = "Add a new task"
    parser = CommandArg <$> (AddTask <$> addDescParser <*> projOptParser <*> dueOptParser now tzone <*> jsonOptParser)

editCommands :: UTCTime -> TimeZone -> (Mod CommandFields Arg, Mod CommandFields Arg)
editCommands now tzone = aliasedCommand parser desc ["edit", "e"]
  where
    desc = "Edit an existing task"
    parser = CommandArg <$> (EditTask <$> idParser <*> editDescParser <*> projOptParser <*> dueOptParser now tzone <*> jsonOptParser)

startCommands :: (Mod CommandFields Arg, Mod CommandFields Arg)
startCommands = aliasedCommand parser desc ["start", "sta", "s"]
  where
    desc = "Start a task"
    parser = CommandArg <$> (StartTask <$> idsParser <*> jsonOptParser)

stopCommands :: (Mod CommandFields Arg, Mod CommandFields Arg)
stopCommands = aliasedCommand parser desc ["stop", "sto", "S"]
  where
    desc = "Stop a task"
    parser = CommandArg <$> (StopTask <$> idsParser <*> jsonOptParser)

toggleCommands :: (Mod CommandFields Arg, Mod CommandFields Arg)
toggleCommands = aliasedCommand parser desc ["toggle", "tog", "t"]
  where
    desc = "Toggle a task"
    parser = CommandArg <$> (ToggleTask <$> idsParser <*> jsonOptParser)

doCommands :: (Mod CommandFields Arg, Mod CommandFields Arg)
doCommands = aliasedCommand parser desc ["done", "do", "d"]
  where
    desc = "Mark as done a task"
    parser = CommandArg <$> (DoTask <$> idsParser <*> jsonOptParser)

undoCommands :: (Mod CommandFields Arg, Mod CommandFields Arg)
undoCommands = aliasedCommand parser desc ["undone", "undo", "u"]
  where
    desc = "Unmark as done a task"
    parser = CommandArg <$> (UndoTask <$> idsParser <*> jsonOptParser)

deleteCommands :: (Mod CommandFields Arg, Mod CommandFields Arg)
deleteCommands = aliasedCommand parser desc ["delete", "del", "D"]
  where
    desc = "Delete a task"
    parser = CommandArg <$> (DeleteTask <$> idsParser <*> jsonOptParser)

undeleteCommands :: (Mod CommandFields Arg, Mod CommandFields Arg)
undeleteCommands = aliasedCommand parser desc ["undelete", "undel", "U"]
  where
    desc = "Undelete a task"
    parser = CommandArg <$> (UndeleteTask <$> idsParser <*> jsonOptParser)

ctxCommands :: (Mod CommandFields Arg, Mod CommandFields Arg)
ctxCommands = aliasedCommand parser desc ["context", "ctx", "c"]
  where
    desc = "Change the current project"
    parser = CommandArg <$> (EditContext <$> projParser <*> jsonOptParser)

-- Procedures

procedures :: Mod CommandFields Arg
procedures =
  foldr1
    (<>)
    [ upgradeProcedure,
      versionProcedure,
      clearCacheProcedure
    ]

upgradeProcedure :: Mod CommandFields Arg
upgradeProcedure = command "upgrade" $ info parser infoMod
  where
    infoMod = progDesc "Upgrade the CLI"
    parser = pure $ ProcedureArg Upgrade

versionProcedure :: Mod CommandFields Arg
versionProcedure = command "version" $ info parser infoMod
  where
    infoMod = progDesc "Show the version"
    parser = ProcedureArg . ShowVersion <$> jsonOptParser

clearCacheProcedure :: Mod CommandFields Arg
clearCacheProcedure = command "cache:clear" $ info parser infoMod
  where
    infoMod = progDesc "Clear the state cache"
    parser = pure $ ProcedureArg ClearCache

-- Readers

readUTCTime :: TimeZone -> String -> Maybe UTCTime
readUTCTime tzone = toUTC <=< parseLocalTime
  where
    parseLocalTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
    toUTC = return . localTimeToUTC tzone

dateReader :: String -> UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
dateReader timefmt now tzone = eitherReader reader
  where
    reader "" = Right Nothing
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
idsParser = some $ argument str $ metavar "IDs..." <> completer idsCompleter

addDescParser :: Parser Desc
addDescParser = unwords <$> some (argument str $ metavar "DESC")

editDescParser :: Parser Desc
editDescParser = unwords <$> many (argument str $ metavar "DESC")

projParser :: Parser Project
projParser = argument projectReader $ metavar "PROJECT" <> value Nothing <> completer projCompleter

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

doneOptParser :: Parser DoneOpt
doneOptParser = switch $ long "done" <> short 'd' <> help "Show only done tasks"

deletedOptParser :: Parser DeletedOpt
deletedOptParser = switch $ long "deleted" <> short 'D' <> help "Show only deleted tasks"

moreOptParser :: String -> Parser MoreOpt
moreOptParser h = switch $ long "more" <> help h

jsonOptParser :: Parser MoreOpt
jsonOptParser = switch $ long "json" <> help "Show result as JSON string"

-- Completers

idsCompleter :: Completer
idsCompleter = mkCompleter idsCompleter'
  where
    idsCompleter' str = sort . filter (isPrefixOf str) . map skipDashes . foldl extractIds [] <$> Store.readFile
    extractIds ids (TaskAdded _ id _ _ _) = ids ++ [id]
    extractIds ids _ = ids

projCompleter :: Completer
projCompleter = mkCompleter projectCompleter'
  where
    projectCompleter' str = sort . nub . filter (isPrefixOf str) . foldl extractProjects [] <$> Store.readFile
    extractProjects projs (TaskAdded _ _ _ (Just proj) _) = projs ++ [proj]
    extractProjects projs (TaskEdited _ _ _ (Just proj) _) = projs ++ [proj]
    extractProjects projs (ContextEdited _ (Just proj)) = projs ++ [proj]
    extractProjects projs _ = projs

-- Helpers

aliasedCommand :: Parser Arg -> String -> [String] -> (Mod CommandFields Arg, Mod CommandFields Arg)
aliasedCommand parser desc (cmd : aliases) = (cmd', aliases')
  where
    cmd' = command cmd $ info parser $ progDesc $ desc ++ aliasesDesc
    aliases' = foldr1 (<>) $ map (`command` info parser idm) aliases
    aliasesDesc
      | null aliases = ""
      | otherwise = " \x1b[38;5;8m[" ++ intercalate ", " aliases ++ "]\x1b[0m"
