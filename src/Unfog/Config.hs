module Unfog.Config
  ( Config,
    getStorePath,
    getReminderCmds,
    readFile,
  )
where

import Control.Applicative ((<|>))
import Data.Text (Text, pack, unpack)
import TOML (Value, parseTOML)
import qualified TOML as Value (Value (..))
import qualified Unfog.File as File
import Prelude hiding (readFile)

-- Model

data Config = Config
  { _storePath :: Maybe String,
    _reminderCmds :: [String]
  }
  deriving (Show, Read, Eq)

new :: Config
new =
  Config
    { _storePath = Nothing,
      _reminderCmds = []
    }

getStorePath :: IO (Maybe String)
getStorePath = _storePath <$> readFile

getReminderCmds :: Config -> [String]
getReminderCmds = _reminderCmds

-- Reducer

reducer :: Config -> (Text, Value) -> Config
reducer config (key, val) = case unpack key of
  "store-path" -> case val of
    Value.String path -> config {_storePath = Just $ unpack path}
  "reminder-cmds" -> case val of
    Value.List list -> config {_reminderCmds = _reminderCmds config ++ parseCmds list}
  "reminder-cmd" -> case val of
    Value.String cmd -> config {_reminderCmds = _reminderCmds config ++ [unpack cmd]}
    _ -> config
  _ -> config

parseCmds :: [Value] -> [String]
parseCmds = concatMap parseCmd
  where
    parseCmd (Value.String str) = [unpack str]
    parseCmd _ = []

-- Reader

readFile :: IO Config
readFile = readTOML <|> return new
  where
    readTOML = parseConfig . parseTOML . pack <$> File.readFromName "config.toml" <|> return new
    parseConfig (Left _) = new
    parseConfig (Right vals) = foldl reducer new vals
