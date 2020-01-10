{-# LANGUAGE NamedFieldPuns #-}

module Parsec where

import           Prelude                 hiding ( Word )
import           Control.Monad
import           Data.Maybe
import           Control.Applicative     hiding ( many
                                                , optional
                                                )
import           Data.Char
import           Data.List
import           Data.Time
import           Text.ParserCombinators.ReadP
import           Text.Printf

data Arg
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

data ArgType = Cmd | Qry deriving (Show, Eq)

data ArgOpts = ArgOpts { _json :: Bool } deriving (Show, Eq)

data ArgDate = ArgDate { _days :: Int
                       , _months :: Int
                       , _years :: Int
                       , _hours :: Int
                       , _mins :: Int
                       } deriving (Show, Eq)

data ArgTree = ArgTree { _type :: ArgType
                       , _cmd :: String
                       , _ids :: [Int]
                       , _desc :: String
                       , _tags :: [String]
                       , _due :: Maybe ArgDate
                       , _minDate :: Maybe ArgDate
                       , _maxDate :: Maybe ArgDate
                       , _opts :: ArgOpts
                       } deriving (Show, Eq)

emptyArgTree :: ArgTree
emptyArgTree = ArgTree { _ids     = []
                       , _type    = Cmd
                       , _cmd     = ""
                       , _desc    = ""
                       , _tags    = []
                       , _due     = Nothing
                       , _minDate = Nothing
                       , _maxDate = Nothing
                       , _opts    = ArgOpts False
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
  , "delete"
  , "remove"
  , "context"
  ]

-- Main exprs

addExpr :: ReadP [Arg]
addExpr = do
  cmd  <- cmdAliasExpr ["add", "a"]
  args <- many1 $ optExpr <++ addTagExpr <++ dueExpr <++ wordExpr
  guard $ isJust $ find (not . isOpt) args
  return $ cmd : args

editExpr :: ReadP [Arg]
editExpr = do
  cmd  <- cmdAliasExpr ["edit", "e"]
  ids  <- many1 idExpr
  args <- many1 $ optExpr <++ addTagExpr <++ delTagExpr <++ dueExpr <++ wordExpr
  return $ cmd : ids ++ args

setExpr :: ReadP [Arg]
setExpr = do
  cmd  <- cmdAliasExpr ["set", "s"]
  ids  <- many1 idExpr
  args <- many1 $ optExpr <++ addTagExpr <++ dueExpr <++ wordExpr
  guard $ isJust $ find (not . isOpt) args
  return $ cmd : ids ++ args

startExpr :: ReadP [Arg]
startExpr = cmdWithIdExpr ["start", "+"]

stopExpr :: ReadP [Arg]
stopExpr = cmdWithIdExpr ["stop", "-"]

toggleExpr :: ReadP [Arg]
toggleExpr = cmdWithIdExpr ["toggle", "t"]

doneExpr :: ReadP [Arg]
doneExpr = cmdWithIdExpr ["done", "d"]

deleteExpr :: ReadP [Arg]
deleteExpr = cmdWithIdExpr ["delete", "D"]

removeExpr :: ReadP [Arg]
removeExpr = cmdWithIdExpr ["remove", "r"]

ctxExpr :: ReadP [Arg]
ctxExpr = do
  cmd  <- cmdAliasExpr ["context", "c"]
  args <- many $ optExpr <++ addCtxTagExpr
  return $ cmd : args

listExpr :: ReadP [Arg]
listExpr = do
  cmd  <- cmdAliasExpr ["list", "l"]
  args <- many optExpr
  return $ cmd : args

infoExpr :: ReadP [Arg]
infoExpr = do
  cmd  <- cmdAliasExpr ["info", "i"]
  id   <- idExpr
  args <- many optExpr
  return $ cmd : id : args

wtimeExpr :: ReadP [Arg]
wtimeExpr = do
  cmd  <- cmdAliasExpr ["worktime", "w"]
  args <- many $ optExpr <++ addCtxTagExpr <++ minDateExpr <++ maxDateExpr
  return $ cmd : args

helpExpr :: ReadP [Arg]
helpExpr = do
  cmd <- cmdAliasExpr ["help", "h", "--help", "-h"]
  return [cmd]

versionExpr :: ReadP [Arg]
versionExpr = do
  cmd  <- cmdAliasExpr ["version", "v", "--version", "-v"]
  rest <- many optExpr
  return $ cmd : rest

upgradeExpr :: ReadP [Arg]
upgradeExpr = do
  cmd <- cmdAliasExpr ["upgrade"]
  return [cmd]

statusExpr :: ReadP [Arg]
statusExpr = do
  cmd  <- cmdAliasExpr ["status"]
  args <- many optExpr
  return $ cmd : args

-- Composite exprs

cmdWithIdExpr :: [String] -> ReadP [Arg]
cmdWithIdExpr aliases = do
  cmd  <- cmdAliasExpr aliases
  ids  <- many1 idExpr
  args <- many optExpr
  return $ cmd : ids ++ args

cmdAliasExpr :: [String] -> ReadP Arg
cmdAliasExpr aliases = do
  skipSpaces
  cmd <- foldr1 (<++) $ map string aliases
  return $ SetCmd $ head aliases

idExpr :: ReadP Arg
idExpr = do
  skipSpaces
  id <- munch1 isDigit
  return $ AddId (read id :: Int)

addTagExpr :: ReadP Arg
addTagExpr = do
  skipSpaces
  char '+'
  tag <- munch1 isTag
  return $ AddTag tag

addCtxTagExpr :: ReadP Arg
addCtxTagExpr = do
  skipSpaces
  optional $ char '+'
  tag <- munch1 isTag
  return $ AddTag tag

delTagExpr :: ReadP Arg
delTagExpr = do
  skipSpaces
  char '-'
  tag <- munch1 isTag
  return $ DelTag tag

int :: Int -> ReadP Int
int c = do
  n <- count c (satisfy isDigit)
  return $ if null n then 0 else read n

dueExpr :: ReadP Arg
dueExpr = SetDue <$> (dateTimeExpr ':' <|> dateExpr ':')

minDateExpr :: ReadP Arg
minDateExpr = SetMinDate <$> (dateTimeExpr '[' <|> dateExpr '[')

maxDateExpr :: ReadP Arg
maxDateExpr = SetMaxDate <$> (dateTimeExpr ']' <|> dateExpr ']')

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
  eof <|> (munch1 (== ' ') >> return ())
  return $ ArgDate days months years hours mins

dateExpr :: Char -> ReadP ArgDate
dateExpr ord = do
  skipSpaces
  char ord
  days   <- int 2 <++ int 1
  months <- int 2 <++ int 1 <++ int 0
  years  <- int 2 <++ int 0
  eof <|> (munch1 (== ' ') >> return ())
  return $ ArgDate days months years 0 0

optExpr :: ReadP Arg
optExpr = do
  skipSpaces
  string "--"
  opt <- choice $ map string ["json"]
  eof <|> (munch1 (== ' ') >> return ())
  return $ AddOpt opt

wordExpr :: ReadP Arg
wordExpr = do
  skipSpaces
  fchar <- get
  rest  <- munch (/= ' ')
  eof <|> (munch1 (== ' ') >> return ())
  return $ AddWord $ fchar : rest

-- Helper

isOpt :: Arg -> Bool
isOpt (AddOpt _) = True
isOpt _          = False

isTag :: Char -> Bool
isTag c | isAlphaNum c  = True
        | c `elem` "-_" = True
        | otherwise     = False

-- Parser

parser :: ReadP [Arg]
parser =
  helpExpr
    <++ versionExpr
    <++ upgradeExpr
    <++ statusExpr
    <++ startExpr
    <++ stopExpr
    <++ toggleExpr
    <++ doneExpr
    <++ deleteExpr
    <++ removeExpr
    <++ ctxExpr
    <++ infoExpr
    <++ listExpr
    <++ wtimeExpr
    <++ editExpr
    <++ setExpr
    <++ addExpr

eval :: ArgTree -> Arg -> ArgTree
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

runParser :: ReadP [Arg] -> String -> ArgTree
runParser p "" = emptyArgTree
runParser p s  = case readP_to_S p s of
  []  -> emptyArgTree
  res -> foldl eval emptyArgTree $ fst . last $ res

parseArgs :: String -> ArgTree
parseArgs = runParser parser

parseDate :: UTCTime -> Int -> Int -> Int -> ArgDate -> Maybe UTCTime
parseDate now defHours defMins defSecs (ArgDate 0 0  0 0 0) = Nothing
parseDate now defHours defMins defSecs (ArgDate d mo y h m) = Just
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

parseDue :: UTCTime -> ArgTree -> Maybe UTCTime
parseDue now args = _due args >>= parseDate now 0 0 0

parseMinDate :: UTCTime -> ArgTree -> Maybe UTCTime
parseMinDate now args = _minDate args >>= parseDate now 0 0 0

parseMaxDate :: UTCTime -> ArgTree -> Maybe UTCTime
parseMaxDate now args = _maxDate args >>= parseDate now 23 59 99
