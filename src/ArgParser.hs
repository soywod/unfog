module ArgParser where

import ArgOptions
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import Options.Applicative
import Task (Desc, Due, Id, Tag)
import Text.ParserCombinators.ReadP hiding (many, option)
import Text.Printf

data Query
  = List OnlyIdsOpt OnlyTagsOpt JsonOpt
  | Info Id JsonOpt
  | Wtime [Tag] FromOpt ToOpt MoreOpt JsonOpt
  | Status MoreOpt JsonOpt
  deriving (Show)

data Command
  = Add CustomArgs JsonOpt
  | Edit Id CustomArgs JsonOpt
  | Set Id CustomArgs JsonOpt
  | Start [Id] JsonOpt
  | Stop [Id] JsonOpt
  | Do [Id] JsonOpt
  | Undo [Id] JsonOpt
  | Delete [Id] JsonOpt
  | Context [Tag] JsonOpt
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
listQuery = command "list" (info parser infoMod)
  where
    infoMod = progDesc "Show tasks filtered by current context"
    parser = QueryArg <$> (List <$> onlyIdsOptParser <*> onlyTagsOptParser <*> jsonOptParser)

infoQuery :: Mod CommandFields Arg
infoQuery = command "info" (info parser infoMod)
  where
    infoMod = progDesc "Show task details"
    parser = QueryArg <$> (Info <$> idParser <*> jsonOptParser)

wtimeQuery :: UTCTime -> TimeZone -> Mod CommandFields Arg
wtimeQuery now tzone = command "worktime" (info parser infoMod)
  where
    infoMod = progDesc "Show task details"
    parser =
      QueryArg
        <$> ( Wtime <$> many (argument str (metavar "TAGS..."))
                <*> fromOptParser now tzone
                <*> toOptParser now tzone
                <*> moreOptParser "Show more details about worktime"
                <*> jsonOptParser
            )

statusQuery :: Mod CommandFields Arg
statusQuery = command "status" (info parser infoMod)
  where
    infoMod = progDesc "Show the total amount of time spent on the current active task"
    parser = QueryArg <$> (Status <$> moreOptParser "Show more details about the task" <*> jsonOptParser)

-- Commands

commands :: UTCTime -> TimeZone -> Mod CommandFields Arg
commands now tzone =
  foldr1
    (<>)
    [ addCommand,
      editCommand,
      setCommand,
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
    parser = CommandArg <$> (Add <$> customArgParser <*> jsonOptParser)

editCommand :: Mod CommandFields Arg
editCommand = command "edit" (info parser infoMod)
  where
    infoMod = progDesc "Edit an existing task"
    parser = CommandArg <$> (Edit <$> idParser <*> customArgParser <*> jsonOptParser)

setCommand :: Mod CommandFields Arg
setCommand = command "set" (info parser infoMod)
  where
    infoMod = progDesc "Replace an existing task"
    parser = CommandArg <$> (Set <$> idParser <*> customArgParser <*> jsonOptParser)

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
doCommand = command "do" (info parser infoMod)
  where
    infoMod = progDesc "Mark as done a task"
    parser = CommandArg <$> (Do <$> idsParser <*> jsonOptParser)

undoCommand :: Mod CommandFields Arg
undoCommand = command "undo" (info parser infoMod)
  where
    infoMod = progDesc "Unmark as done a task"
    parser = CommandArg <$> (Undo <$> idsParser <*> jsonOptParser)

deleteCommand :: Mod CommandFields Arg
deleteCommand = command "delete" (info parser infoMod)
  where
    infoMod = progDesc "Delete a task"
    parser = CommandArg <$> (Delete <$> idsParser <*> jsonOptParser)

ctxCommand :: Mod CommandFields Arg
ctxCommand = command "context" (info parser infoMod)
  where
    infoMod = progDesc "Change the current context"
    parser = CommandArg <$> (Context <$> tagsParser <*> jsonOptParser)

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
readUTCTime tzone = parseLocalTime >=> toUTC
  where
    parseLocalTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%s"
    toUTC = return . localTimeToUTC tzone

dateReader :: String -> UTCTime -> TimeZone -> ReadM (Maybe UTCTime)
dateReader timefmt now tzone = maybeReader parseDate
  where
    parseDate str = Just $ parseDate' <|> parseTime' <|> parseDateTime'
      where
        parseDate' = readUTCTime tzone (str ++ " " ++ timefmt)
        parseTime' = readUTCTime tzone (formatTime defaultTimeLocale "%Y-%m-%d " now ++ str)
        parseDateTime' = readUTCTime tzone str

fromDateReader :: UTCTime -> TimeZone -> ReadM FromOpt
fromDateReader = dateReader "00:00:00"

toDateReader :: UTCTime -> TimeZone -> ReadM ToOpt
toDateReader = dateReader "23:59:59"

-- Parsers

idParser :: Parser Id
idParser = argument str (metavar "ID")

idsParser :: Parser [Id]
idsParser = some idParser

customArgParser :: Parser CustomArgs
customArgParser = parseCustomArgs <$> many (argument str (metavar "ARGS..."))

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

-- Custom parsers

type CustomArgs = ([String], [Tag], Maybe ArgDate)

runCustomParser :: ReadP [CustomArgExpr] -> String -> CustomArgs
runCustomParser p "" = ([], [], Nothing)
runCustomParser p s = case readP_to_S p s of
  [] -> ([], [], Nothing)
  res -> foldl eval ([], [], Nothing) $ fst . last $ res

eval :: CustomArgs -> CustomArgExpr -> CustomArgs
eval (desc, tags, due) arg = case arg of
  AddTag tag -> (desc, tags ++ [tag], due)
  DelTag tag -> (desc, tags \\ [tag], due)
  AddWord word -> (desc ++ [word], tags, due)
  SetDue due -> (desc, tags, Just due)

parseCustomArgs :: [String] -> CustomArgs
parseCustomArgs = runCustomParser (setExpr <++ editExpr) . unwords

data CustomArgExpr
  = AddTag String
  | DelTag String
  | AddWord String
  | SetDue ArgDate
  deriving (Show)

data ArgDateType = Rel | Abs deriving (Show, Eq)

data ArgDate = ArgDate
  { _dateType :: ArgDateType,
    _months :: Int,
    _days :: Int,
    _years :: Int,
    _hours :: Int,
    _mins :: Int
  }
  deriving (Show, Eq)

parseDate :: UTCTime -> TimeZone -> Int -> Int -> Maybe ArgDate -> Maybe UTCTime
parseDate now tzone defHours defMins Nothing = Nothing
parseDate now tzone defHours defMins (Just (ArgDate Rel d mo y h m)) =
  readUTCTime tzone $ unwords [dateStr, timeStr]
  where
    (y', mo', d') = toGregorian $ utctDay now
    mins = if m > 0 then m else defMins
    hours = if h > 0 then h else defHours
    days = if d > 0 then d else d'
    months = if mo > 0 then mo else mo'
    years =
      if y == 0
        then fromInteger y'
        else if y < 100 then y + truncate (realToFrac y' / 100) * 100 else y
    dateStr = printf "%.4d-%.2d-%.2d" years months days
    timeStr = printf "%.2d:%.2d:%.2d" hours mins defMins

setExpr :: ReadP [CustomArgExpr]
setExpr = do
  args <- many1 $ addTagExpr <++ dueExpr <++ wordExpr
  guard $ isJust $ find hasAddWordExpr args
  return args

editExpr :: ReadP [CustomArgExpr]
editExpr = many1 $ addTagExpr <++ dueExpr <++ wordExpr

wordExpr :: ReadP CustomArgExpr
wordExpr = do
  skipSpaces
  fchar <- get
  rest <- munch (/= ' ')
  eofOrSpaces
  return $ AddWord $ fchar : rest

addTagExpr :: ReadP CustomArgExpr
addTagExpr = do
  skipSpaces
  char '+'
  tag <- munch1 isTag
  return $ AddTag tag

isTag :: Char -> Bool
isTag c
  | isAlphaNum c = True
  | c `elem` "-_" = True
  | otherwise = False

hasAddWordExpr :: CustomArgExpr -> Bool
hasAddWordExpr (AddWord _) = True
hasAddWordExpr _ = False

dueExpr :: ReadP CustomArgExpr
dueExpr = SetDue <$> (dateTimeExpr ':' <++ dateExpr ':')

dateTimeExpr :: Char -> ReadP ArgDate
dateTimeExpr ord = do
  skipSpaces
  char ord
  days <- int 2 <++ int 1 <++ int 0
  months <- int 2 <++ int 1 <++ int 0
  years <- int 2 <++ int 0
  char ':'
  hours <- int 2 <++ int 1 <++ int 0
  mins <- int 2 <++ int 1 <++ int 0
  eofOrSpaces
  return $ ArgDate Rel days months years hours mins

dateExpr :: Char -> ReadP ArgDate
dateExpr ord = do
  skipSpaces
  char ord
  days <- int 2 <++ int 1
  months <- int 2 <++ int 1 <++ int 0
  years <- int 2 <++ int 0
  eofOrSpaces
  return $ ArgDate Rel days months years 0 0

int :: Int -> ReadP Int
int c = do
  n <- count c (satisfy isDigit)
  return $ if null n then 0 else read n

eofOrSpaces :: ReadP ()
eofOrSpaces = eof <++ atLeastOneSpace
  where
    atLeastOneSpace = munch1 (== ' ') >> return ()
