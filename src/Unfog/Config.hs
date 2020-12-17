module Unfog.Config
  ( Config,
    getStorePath,
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

newtype Config = Config
  { _storePath :: Maybe String
  }
  deriving (Show, Read, Eq)

new :: Config
new =
  Config
    { _storePath = Nothing
    }

getStorePath :: IO (Maybe String)
getStorePath = _storePath <$> readFile

-- Reducer

reducer :: Config -> (Text, Value) -> Config
reducer config (key, val) = case unpack key of
  "store-path" -> case val of
    Value.String path -> config {_storePath = Just $ unpack path}
    _ -> config
  _ -> config

-- Reader

readFile :: IO Config
readFile = readTOML <|> return new
  where
    readTOML = parseConfig . parseTOML . pack <$> File.readFromName "config.toml" <|> return new
    parseConfig (Left _) = new
    parseConfig (Right vals) = foldl reducer new vals
