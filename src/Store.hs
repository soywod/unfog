module Store where

import qualified Config
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import qualified Event.MigrationV0 as V0
import Event.Type (Event, readEvents)
import qualified File

getFilePath :: IO String
getFilePath = do
  pathFromTOML <- Config.getStorePath
  defaultPath <- File.getPath "store"
  return $ fromMaybe defaultPath pathFromTOML

readFile :: IO [Event]
readFile = do
  fpath <- getFilePath
  handleEvts <- foldr readEvents (Just []) . lines <$> (File.readFromPath fpath <|> return "")
  handleEvtsV0 <- V0.handleEvts
  let evts = fromMaybe [] (handleEvts <|> handleEvtsV0)
  if null evts then return () else Store.writeFile evts
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
