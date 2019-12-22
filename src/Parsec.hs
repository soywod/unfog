{-# LANGUAGE NamedFieldPuns #-}

module Parsec where

import           Prelude                 hiding ( Word )
import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.List
import           Data.Time
import           Text.ParserCombinators.ReadP
import           Text.Printf

data Arg
  = SetCmd String
  | SetId Int
  | AddTag String
  | DelTag String
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
                       , _id :: Int
                       , _desc :: String
                       , _tags :: [String]
                       , _minDate :: Maybe ArgDate
                       , _maxDate :: Maybe ArgDate
                       , _opts :: ArgOpts
                       } deriving (Show, Eq)

emptyArgTree :: ArgTree
emptyArgTree = ArgTree { _id      = 0
                       , _type    = Cmd
                       , _cmd     = ""
                       , _desc    = ""
                       , _tags    = []
                       , _minDate = Nothing
                       , _maxDate = Nothing
                       , _opts    = ArgOpts False
                       }

queries = ["list", "show", "worktime", "help", "version"]
commands =
  [ "create"
  , "update"
  , "replace"
  , "start"
  , "stop"
  , "toggle"
  , "done"
  , "delete"
  , "remove"
  , "context"
  ]

-- Main exprs

createExpr :: ReadP [Arg]
createExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["create", "add"]
  skipSpaces
  rest <-
    sepBySpaces1
    $   (AddWord <$> wordExpr)
    <|> (AddTag <$> addTagExpr)
    <|> (AddOpt <$> optExpr)
  skipSpaces
  return $ cmd : rest

updateExpr :: ReadP [Arg]
updateExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["update", "edit"]
  skipSpaces
  id <- SetId <$> idExpr
  skipSpaces
  rest <-
    sepBySpaces1
    $   (AddWord <$> wordExpr)
    <|> (AddTag <$> addTagExpr)
    <|> (DelTag <$> delTagExpr)
    <|> (AddOpt <$> optExpr)
  skipSpaces
  return $ cmd : id : rest

replaceExpr :: ReadP [Arg]
replaceExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["replace", "set"]
  skipSpaces
  id <- SetId <$> idExpr
  skipSpaces
  rest <-
    sepBySpaces1
    $   (AddWord <$> wordExpr)
    <|> (AddTag <$> addTagExpr)
    <|> (AddOpt <$> optExpr)
  skipSpaces
  return $ cmd : id : rest

startExpr :: ReadP [Arg]
startExpr = cmdWithIdExpr ["start"]

stopExpr :: ReadP [Arg]
stopExpr = cmdWithIdExpr ["stop"]

toggleExpr :: ReadP [Arg]
toggleExpr = cmdWithIdExpr ["toggle"]

doneExpr :: ReadP [Arg]
doneExpr = cmdWithIdExpr ["done"]

deleteExpr :: ReadP [Arg]
deleteExpr = cmdWithIdExpr ["delete"]

removeExpr :: ReadP [Arg]
removeExpr = cmdWithIdExpr ["remove"]

contextExpr :: ReadP [Arg]
contextExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["context", "ctx"]
  skipSpaces
  rest <- sepBySpaces $ (AddTag <$> addTagExpr) <|> (AddOpt <$> optExpr)
  skipSpaces
  return $ cmd : rest

listExpr :: ReadP [Arg]
listExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["list"]
  skipSpaces
  rest <- sepBySpaces $ (AddOpt <$> optExpr)
  skipSpaces
  return $ cmd : rest

showExpr :: ReadP [Arg]
showExpr = cmdWithIdExpr ["show"]

worktimeExpr :: ReadP [Arg]
worktimeExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["worktime", "wtime"]
  skipSpaces
  rest <-
    sepBySpaces
    $   (AddTag <$> addTagExpr)
    <|> (SetMinDate <$> minDateExpr)
    <|> (SetMaxDate <$> maxDateExpr)
    <|> (AddOpt <$> optExpr)
  skipSpaces
  return $ cmd : rest

helpExpr :: ReadP [Arg]
helpExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["help", "h", "--help", "-h"]
  skipSpaces
  return [cmd]

versionExpr :: ReadP [Arg]
versionExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["version", "v", "--version", "-v"]
  skipSpaces
  rest <- sepBySpaces $ AddOpt <$> optExpr
  skipSpaces
  return $ cmd : rest

-- Composite exprs

sepBySpaces :: ReadP Arg -> ReadP [Arg]
sepBySpaces args = args `sepBy` skipSpaces

sepBySpaces1 :: ReadP Arg -> ReadP [Arg]
sepBySpaces1 args = args `sepBy1` skipSpaces

cmdWithIdExpr :: [String] -> ReadP [Arg]
cmdWithIdExpr aliases = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr aliases
  skipSpaces
  id <- SetId <$> idExpr
  skipSpaces
  rest <- many $ AddOpt <$> optExpr
  return $ cmd : id : rest

cmdAliasExpr :: [String] -> ReadP String
cmdAliasExpr aliases = do
  skipSpaces
  cmd <- choice $ map string aliases
  skipSpaces
  return $ head aliases

idExpr :: ReadP Int
idExpr = do
  skipSpaces
  id <- munch1 isDigit
  skipSpaces
  return (read id :: Int)

addTagExpr :: ReadP String
addTagExpr = do
  char '+'
  tag <- munch1 isAlphaNum'
  return tag

delTagExpr :: ReadP String
delTagExpr = do
  char '-'
  tag <- munch1 isAlphaNum'
  return tag

numbers :: Int -> ReadP Int
numbers c = do
  n <- count c (satisfy isDigit)
  return $ if null n then 0 else read n

minDateExpr :: ReadP ArgDate
minDateExpr = dateWithTimeExpr '[' <|> dateWithoutTimeExpr '['

maxDateExpr :: ReadP ArgDate
maxDateExpr = dateWithTimeExpr ']' <|> dateWithoutTimeExpr ']'

dateWithTimeExpr :: Char -> ReadP ArgDate
dateWithTimeExpr ord = do
  char ord
  days   <- numbers 0 <|> numbers 1 <|> numbers 2
  months <- numbers 0 <|> numbers 1 <|> numbers 2
  years  <- numbers 0 <|> numbers 2
  char ':'
  hours <- numbers 0 <|> numbers 1 <|> numbers 2
  mins  <- numbers 0 <|> numbers 1 <|> numbers 2
  return $ ArgDate days months years hours mins

dateWithoutTimeExpr :: Char -> ReadP ArgDate
dateWithoutTimeExpr ord = do
  char ord
  days   <- numbers 1 <|> numbers 2
  months <- numbers 0 <|> numbers 1 <|> numbers 2
  years  <- numbers 0 <|> numbers 2
  return $ ArgDate days months years 0 0

optExpr :: ReadP String
optExpr = do
  string "--"
  opt <- choice $ map string ["json"]
  return opt

wordExpr :: ReadP String
wordExpr = munch1 isAlphaNum'

-- Helper

isAlphaNum' :: Char -> Bool
isAlphaNum' c | isAlphaNum c  = True
              | c `elem` "-_" = True
              | otherwise     = False

-- Parser

parser :: ReadP [Arg]
parser =
  createExpr
    <|> updateExpr
    <|> replaceExpr
    <|> startExpr
    <|> stopExpr
    <|> toggleExpr
    <|> doneExpr
    <|> deleteExpr
    <|> removeExpr
    <|> contextExpr
    <|> listExpr
    <|> showExpr
    <|> worktimeExpr
    <|> helpExpr
    <|> versionExpr

eval :: ArgTree -> Arg -> ArgTree
eval tree arg = case arg of
  SetCmd _cmd -> tree { _cmd, _type }
   where
    _type | _cmd `elem` commands = Cmd
          | _cmd `elem` queries  = Qry
  SetId   _id  -> tree { _id }
  AddTag  tag  -> tree { _tags = _tags tree ++ [tag] }
  DelTag  tag  -> tree { _tags = _tags tree \\ [tag] }
  AddWord desc -> tree { _desc = desc' }
    where desc' = if null (_desc tree) then desc else _desc tree ++ " " ++ desc
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

parseMinDate :: UTCTime -> ArgTree -> Maybe UTCTime
parseMinDate now args = _minDate args >>= parseDate now 0 0 0

parseMaxDate :: UTCTime -> ArgTree -> Maybe UTCTime
parseMaxDate now args = _maxDate args >>= parseDate now 23 59 99
