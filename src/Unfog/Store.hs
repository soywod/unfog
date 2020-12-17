module Unfog.Store where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import qualified Unfog.Config as Config
import qualified Unfog.Event.MigrationV0 as V0
import Unfog.Event.Type (Event, readEvents)
import qualified Unfog.File as File

fileName = "store"

getFilePath :: IO String
getFilePath = do
  pathFromTOML <- Config.getStorePath
  defaultPath <- File.getFullPath fileName
  return $ fromMaybe defaultPath pathFromTOML

readFile :: IO [Event]
readFile = do
  fpath <- getFilePath
  handleEvts <- foldr readEvents (Just []) . lines <$> (File.readFromPath fpath <|> return "")
  handleEvtsV0 <- V0.handleEvts
  let evts = fromMaybe [] (handleEvts <|> handleEvtsV0)
  if null evts then return () else Unfog.Store.writeFile evts
  return evts

writeFile :: [Event] -> IO ()
writeFile evts = writeFile' evts' =<< getFilePath
  where
    evts' = unlines $ map show evts
    writeFile' = flip Prelude.writeFile

appendFile :: [Event] -> IO ()
appendFile = mapM_ writeEvent
  where
    writeEvent evt = appendToStore evt =<< getFilePath
    appendToStore evt store = Prelude.appendFile store $ show evt ++ "\n"
