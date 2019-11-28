module Main where

import           Prelude                 hiding ( Word )
import           Data.List                      ( (\\)
                                                , intercalate
                                                )
import           System.Environment             ( getArgs )

import qualified Command                       as C
import qualified Query                         as Q
import           Data.Char
import           Response
import           Control.Applicative     hiding ( many )
import           Text.ParserCombinators.ReadP

data Arg
  = Cmd String
  | AddTag String
  | DelTag String
  | Desc String
  deriving Show

type Cmd = String
type Word = String
type Tag = String
data State = State { _cmd :: Cmd, _desc :: [Word], _tags :: [Tag] } deriving Show
emptyState = State { _cmd = "help", _desc = [], _tags = [] }

isAlphaNumOrDash c | isAlphaNum c  = True
                   | c `elem` "-_" = True
                   | otherwise     = False

cmdExpr = string "cmd1" <|> string "cmd2"

addTagExpr = do
  satisfy (== '+')
  tag <- munch1 isAlphaNumOrDash
  skipSpaces
  return tag

delTagExpr = do
  satisfy (== '-')
  tag <- munch1 isAlphaNumOrDash
  skipSpaces
  return tag

wordExpr = do
  fchar <- satisfy $ not . flip elem "+-"
  word  <- munch1 (/= ' ')
  skipSpaces
  return $ fchar : word

parser :: ReadP [Arg]
parser = do
  cmd <- Cmd <$> cmdExpr
  skipSpaces
  args <- many1 $ addTag <|> delTag <|> word
  return $ cmd : args
 where
  addTag = AddTag <$> addTagExpr
  delTag = DelTag <$> delTagExpr
  word   = Desc <$> wordExpr

eval :: State -> Arg -> State
eval state arg = case arg of
  Cmd    cmd  -> state { _cmd = cmd }
  AddTag tag  -> state { _tags = _tags state ++ [tag] }
  DelTag tag  -> state { _tags = _tags state \\ [tag] }
  Desc   desc -> state { _desc = _desc state ++ [desc] }

runParser :: ReadP [Arg] -> String -> State
runParser p "" = error "Command missing."
runParser p s  = case readP_to_S p s of
  []  -> error "Command not found"
  res -> foldl eval emptyState $ fst . last $ res

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch args = case args \\ options of
  ("create"   : args) -> C.handle rtype $ "create" : args
  ("add"      : args) -> C.handle rtype $ "create" : args
  ("update"   : args) -> C.handle rtype $ "update" : args
  ("edit"     : args) -> C.handle rtype $ "update" : args
  ("replace"  : args) -> C.handle rtype $ "replace" : args
  ("start"    : args) -> C.handle rtype $ "start" : args
  ("stop"     : args) -> C.handle rtype $ "stop" : args
  ("toggle"   : args) -> C.handle rtype $ "toggle" : args
  ("done"     : args) -> C.handle rtype $ "done" : args
  ("delete"   : args) -> C.handle rtype $ "delete" : args
  ("remove"   : args) -> C.handle rtype $ "remove" : args
  ("context"  : args) -> C.handle rtype $ "context" : args

  ("list"     : args) -> Q.handle rtype $ "list" : args
  ("show"     : args) -> Q.handle rtype $ "show" : args
  ("wtime"    : args) -> Q.handle rtype $ "worktime" : args
  ("worktime" : args) -> Q.handle rtype $ "worktime" : args

  (command    : _   ) -> printErr rtype $ command ++ ": command not found"
  []                  -> printErr rtype "command missing"
 where
  options = ["--json"]
  rtype   = getResponseType args
