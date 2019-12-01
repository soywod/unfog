{-# LANGUAGE NamedFieldPuns #-}

module Parsec where

import           Prelude                 hiding ( Word )
import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.List
import           System.Environment
import           Text.ParserCombinators.ReadP

data Arg
  = SetCmd String
  | SetId Int
  | AddTag String
  | DelTag String
  | AddWord String
  | AddOpt String
  deriving Show

data ArgType = Cmd | Qry deriving (Show, Eq)
data ArgOpts = ArgOpts { _json :: Bool } deriving (Show, Eq)
data ArgTree = ArgTree { _type :: ArgType
                       , _cmd :: String
                       , _id :: Int
                       , _desc :: String
                       , _tags :: [String]
                       , _opts :: ArgOpts
                       } deriving (Show, Eq)


emptyArgTree :: ArgTree
emptyArgTree = ArgTree { _id   = 0
                       , _type = Cmd
                       , _cmd  = ""
                       , _desc = ""
                       , _tags = []
                       , _opts = ArgOpts False
                       }

queries = ["list", "show", "worktime"]
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
    many1
    $   (AddTag <$> addTagExpr)
    <|> (AddWord <$> wordExpr)
    <|> (AddOpt <$> optExpr)
  return $ cmd : rest

updateExpr :: ReadP [Arg]
updateExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["update", "edit"]
  skipSpaces
  id <- SetId <$> idExpr
  skipSpaces
  rest <-
    many1
    $   (AddTag <$> addTagExpr)
    <|> (DelTag <$> delTagExpr)
    <|> (AddWord <$> wordExpr)
    <|> (AddOpt <$> optExpr)
  return $ cmd : id : rest

replaceExpr :: ReadP [Arg]
replaceExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["replace", "set"]
  skipSpaces
  id <- SetId <$> idExpr
  skipSpaces
  rest <-
    many1
    $   (AddTag <$> addTagExpr)
    <|> (AddWord <$> wordExpr)
    <|> (AddOpt <$> optExpr)
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
  rest <- many $ (AddTag <$> addTagExpr) <|> (AddOpt <$> optExpr)
  return $ cmd : rest

listExpr :: ReadP [Arg]
listExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["list"]
  skipSpaces
  rest <- many $ (AddOpt <$> optExpr)
  return $ cmd : rest

showExpr :: ReadP [Arg]
showExpr = cmdWithIdExpr ["show"]

worktimeExpr :: ReadP [Arg]
worktimeExpr = do
  skipSpaces
  cmd <- SetCmd <$> cmdAliasExpr ["worktime", "wtime"]
  skipSpaces
  rest <- many $ (AddTag <$> addTagExpr) <|> (AddOpt <$> optExpr)
  return $ cmd : rest

-- Composite exprs

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
  cmd <- choice $ map string aliases
  return $ head aliases

idExpr :: ReadP Int
idExpr = do
  id <- munch1 isDigit
  return (read id :: Int)

addTagExpr :: ReadP String
addTagExpr = do
  satisfy (== '+')
  tag <- munch1 isAlphaNumOrDash
  skipSpaces
  return tag

delTagExpr :: ReadP String
delTagExpr = do
  satisfy (== '-')
  tag <- munch1 isAlphaNumOrDash
  skipSpaces
  return tag

wordExpr :: ReadP String
wordExpr = do
  fchar <- satisfy $ not . flip elem "+-"
  word  <- munch (/= ' ')
  skipSpaces
  return $ fchar : word

optExpr :: ReadP String
optExpr = do
  string "--"
  opt <- choice $ map string ["json"]
  skipSpaces
  return opt

-- Helper

isAlphaNumOrDash :: Char -> Bool
isAlphaNumOrDash c | isAlphaNum c  = True
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
  AddOpt opt -> tree { _opts = opts }
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
