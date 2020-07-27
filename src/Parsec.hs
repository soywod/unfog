{-# LANGUAGE NamedFieldPuns #-}

module Parsec where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Time
import           Text.ParserCombinators.ReadP
import           Text.Printf

data ArgExpr
  = SetCmd String
  | AddId Int
  | AddTag String
  | DelTag String
  | SetDue ArgDate
  | SetMinDate ArgDate
  | SetMaxDate ArgDate
  | AddWord String
  | AddOpt String
  deriving Show

data ArgDateType = Rel | Abs deriving (Show, Eq)
data ArgDate = ArgDate { _dateType :: ArgDateType
                       , _months :: Int
                       , _days :: Int
                       , _years :: Int
                       , _hours :: Int
                       , _mins :: Int
                       } deriving (Show, Eq)

data ArgType = Cmd | Qry deriving (Show, Eq)

data ArgOpts = ArgOpts { _json :: Bool
                       , _more :: Bool
                       , _onlyIds :: Bool
                       , _onlyTags :: Bool
                       } deriving (Show, Eq)

data Arg = Arg { _type :: ArgType
               , _cmd :: String
               , _ids :: [Int]
               , _desc :: String
               , _tags :: [String]
               , _due :: Maybe ArgDate
               , _minDate :: Maybe ArgDate
               , _maxDate :: Maybe ArgDate
               , _opts :: ArgOpts
               } deriving (Show, Eq)

emptyArgTree :: Arg
emptyArgTree = Arg { _ids     = []
                   , _type    = Qry
                   , _cmd     = "list"
                   , _desc    = ""
                   , _tags    = []
                   , _due     = Nothing
                   , _minDate = Nothing
                   , _maxDate = Nothing
                   , _opts    = ArgOpts False False False False
                   }

queries = ["list", "info", "worktime", "status", "upgrade", "version", "help"]
commands =
  [ "add"
  , "edit"
  , "set"
  , "start"
  , "stop"
  , "toggle"
  , "done"
  , "undone"
  , "delete"
  , "remove"
  , "context"
  ]

-- Main exprs

addExpr :: ReadP [ArgExpr]
addExpr = do
  cmd  <- cmdAliasExpr ["add", "a"]
  args <- many1 $ optExpr <++ addTagExpr <++ dueExpr <++ wordExpr
  guard $ isJust $ find (not . isOpt) args
  return $ cmd : args

editExpr :: ReadP [ArgExpr]
editExpr = do
  cmd  <- cmdAliasExpr ["edit", "e"]
  ids  <- many1 idExpr
  args <- many1 $ optExpr <++ addTagExpr <++ delTagExpr <++ dueExpr <++ wordExpr
  return $ cmd : ids ++ args

setExpr :: ReadP [ArgExpr]
setExpr = do
  cmd  <- cmdAliasExpr ["set", "s"]
  ids  <- many1 idExpr
  args <- many1 $ optExpr <++ addTagExpr <++ dueExpr <++ wordExpr
  guard $ isJust $ find (not . isOpt) args
  return $ cmd : ids ++ args

startExpr :: ReadP [ArgExpr]
startExpr = cmdWithIdExpr ["start", "+"]

stopExpr :: ReadP [ArgExpr]
stopExpr = cmdWithIdExpr ["stop", "-"]

toggleExpr :: ReadP [ArgExpr]
toggleExpr = cmdWithIdExpr ["toggle", "t"]

doneExpr :: ReadP [ArgExpr]
doneExpr = cmdWithIdExpr ["done", "d"]

undoneExpr :: ReadP [ArgExpr]
undoneExpr = cmdWithIdExpr ["undone", "u"]

deleteExpr :: ReadP [ArgExpr]
deleteExpr = cmdWithIdExpr ["delete", "D"]

removeExpr :: ReadP [ArgExpr]
removeExpr = cmdWithIdExpr ["remove", "r"]

ctxExpr :: ReadP [ArgExpr]
ctxExpr = do
  cmd  <- cmdAliasExpr ["context", "c"]
  args <- many $ optExpr <++ addCtxTagExpr
  return $ cmd : args

listExpr :: ReadP [ArgExpr]
listExpr = do
  cmd  <- cmdAliasExpr ["list", "l"]
  args <- many $ listOptExpr <++ optExpr
  return $ cmd : args

listOptExpr :: ReadP ArgExpr
listOptExpr = do
  skipSpaces
  opt <- choice $ map string ["ids", "tags"]
  eofOrSpaces
  return $ AddOpt opt

infoExpr :: ReadP [ArgExpr]
infoExpr = do
  cmd  <- cmdAliasExpr ["info", "i"]
  id   <- idExpr
  args <- many optExpr
  return $ cmd : id : args

wtimeExpr :: ReadP [ArgExpr]
wtimeExpr = do
  cmd  <- cmdAliasExpr ["worktime", "w"]
  args <- many $ optExpr <++ addCtxTagExpr <++ minDateExpr <++ maxDateExpr
  return $ cmd : args

helpExpr :: ReadP [ArgExpr]
helpExpr = do
  cmd <- cmdAliasExpr ["help", "h", "--help", "-h"]
  return [cmd]

versionExpr :: ReadP [ArgExpr]
versionExpr = do
  cmd  <- cmdAliasExpr ["version", "v", "--version", "-v"]
  rest <- many optExpr
  return $ cmd : rest

upgradeExpr :: ReadP [ArgExpr]
upgradeExpr = do
  cmd <- cmdAliasExpr ["upgrade"]
  return [cmd]

statusExpr :: ReadP [ArgExpr]
statusExpr = do
  cmd  <- cmdAliasExpr ["status"]
  args <- many optExpr
  return $ cmd : args

-- Composite exprs

cmdWithIdExpr :: [String] -> ReadP [ArgExpr]
cmdWithIdExpr aliases = do
  cmd  <- cmdAliasExpr aliases
  ids  <- many1 idExpr
  args <- many optExpr
  return $ cmd : ids ++ args

cmdAliasExpr :: [String] -> ReadP ArgExpr
cmdAliasExpr aliases = do
  skipSpaces
  cmd <- foldr1 (<++) $ map string aliases
  return $ SetCmd $ head aliases

idExpr :: ReadP ArgExpr
idExpr = do
  skipSpaces
  id <- munch1 isDigit
  return $ AddId (read id :: Int)

addTagExpr :: ReadP ArgExpr
addTagExpr = do
  skipSpaces
  char '+'
  tag <- munch1 isTag
  return $ AddTag tag

addCtxTagExpr :: ReadP ArgExpr
addCtxTagExpr = do
  skipSpaces
  optional $ char '+'
  tag <- munch1 isTag
  return $ AddTag tag

delTagExpr :: ReadP ArgExpr
delTagExpr = do
  skipSpaces
  char '-'
  tag <- munch1 isTag
  return $ DelTag tag

dueExpr :: ReadP ArgExpr
dueExpr = SetDue <$> (dateTimeExpr ':' <++ dateExpr ':')

minDateExpr :: ReadP ArgExpr
minDateExpr = SetMinDate <$> (dateTimeExpr '[' <++ dateExpr '[')

maxDateExpr :: ReadP ArgExpr
maxDateExpr = SetMaxDate <$> (dateTimeExpr ']' <++ dateExpr ']')

dateTimeExpr :: Char -> ReadP ArgDate
dateTimeExpr ord = do
  skipSpaces
  char ord
  days   <- int 2 <++ int 1 <++ int 0
  months <- int 2 <++ int 1 <++ int 0
  years  <- int 2 <++ int 0
  char ':'
  hours <- int 2 <++ int 1 <++ int 0
  mins  <- int 2 <++ int 1 <++ int 0
  eofOrSpaces
  return $ ArgDate Rel days months years hours mins

dateExpr :: Char -> ReadP ArgDate
dateExpr ord = do
  skipSpaces
  char ord
  days   <- int 2 <++ int 1
  months <- int 2 <++ int 1 <++ int 0
  years  <- int 2 <++ int 0
  eofOrSpaces
  return $ ArgDate Rel days months years 0 0

optExpr :: ReadP ArgExpr
optExpr = do
  skipSpaces
  string "--"
  opt <- choice $ map string ["json", "more"]
  eofOrSpaces
  return $ AddOpt opt

wordExpr :: ReadP ArgExpr
wordExpr = do
  skipSpaces
  fchar <- get
  rest  <- munch (/= ' ')
  eofOrSpaces
  return $ AddWord $ fchar : rest

-- Helper

isOpt :: ArgExpr -> Bool
isOpt (AddOpt _) = True
isOpt _          = False

isTag :: Char -> Bool
isTag c | isAlphaNum c  = True
        | c `elem` "-_" = True
        | otherwise     = False

int :: Int -> ReadP Int
int c = do
  n <- count c (satisfy isDigit)
  return $ if null n then 0 else read n

eofOrSpaces :: ReadP ()
eofOrSpaces = eof <++ atLeastOnSpace
  where atLeastOnSpace = munch1 (== ' ') >> return ()

-- Parser

parseArgs :: String -> Arg
parseArgs =
  runParser
    $   helpExpr
    <++ versionExpr
    <++ upgradeExpr
    <++ statusExpr
    <++ startExpr
    <++ stopExpr
    <++ toggleExpr
    <++ doneExpr
    <++ undoneExpr
    <++ deleteExpr
    <++ removeExpr
    <++ ctxExpr
    <++ infoExpr
    <++ listExpr
    <++ wtimeExpr
    <++ editExpr
    <++ setExpr
    <++ addExpr

parseDate :: UTCTime -> Int -> Int -> Int -> ArgDate -> Maybe UTCTime
parseDate now defHours defMins defSecs (ArgDate Rel 0 0  0 0 0) = Nothing
parseDate now defHours defMins defSecs (ArgDate Rel d mo y h m) = Just
  (read $ unwords [dateStr, timeStr] :: UTCTime)
 where
  (y', mo', d') = toGregorian $ utctDay now
  mins          = if m > 0 then m else defMins
  hours         = if h > 0 then h else defHours
  days          = if d > 0 then d else d'
  months        = if mo > 0 then mo else mo'
  years         = if y == 0
    then fromInteger y'
    else if y < 100 then y + truncate (realToFrac y' / 100) * 100 else y
  dateStr = printf "%.4d-%.2d-%.2d" years months days
  timeStr = printf "%.2d:%.2d:%.2d" hours mins defSecs

parseDue :: UTCTime -> Arg -> Maybe UTCTime
parseDue now args = _due args >>= parseDate now 0 0 0

parseMinDate :: UTCTime -> Arg -> Maybe UTCTime
parseMinDate now args = _minDate args >>= parseDate now 0 0 0

parseMaxDate :: UTCTime -> Arg -> Maybe UTCTime
parseMaxDate now args = _maxDate args >>= parseDate now 23 59 99

runParser :: ReadP [ArgExpr] -> String -> Arg
runParser p "" = emptyArgTree
runParser p s  = case readP_to_S p s of
  []  -> emptyArgTree
  res -> foldl eval emptyArgTree $ fst . last $ res

eval :: Arg -> ArgExpr -> Arg
eval tree arg = case arg of
  SetCmd _cmd -> tree { _cmd, _type }
   where
    _type | _cmd `elem` commands = Cmd
          | _cmd `elem` queries  = Qry
  AddId   id   -> tree { _ids = _ids tree ++ [id] }
  AddTag  tag  -> tree { _tags = _tags tree ++ [tag] }
  DelTag  tag  -> tree { _tags = _tags tree \\ [tag] }
  AddWord desc -> tree { _desc = desc' }
    where desc' = if null (_desc tree) then desc else _desc tree ++ " " ++ desc
  SetDue     due -> tree { _due = Just due }
  SetMinDate min -> tree { _minDate = Just min }
  SetMaxDate max -> tree { _maxDate = Just max }
  AddOpt     opt -> tree { _opts = opts }
   where
    opts = case opt of
      "json" -> (_opts tree) { _json = True }
      "more" -> (_opts tree) { _more = True }
      "ids"  -> (_opts tree) { _onlyIds = True }
      "tags" -> (_opts tree) { _onlyTags = True }
