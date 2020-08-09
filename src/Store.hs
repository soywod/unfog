module Store where

import Control.Applicative ((<|>))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import qualified Event.MigrationV0 as V0 (handleEvts)
import Event.Type (Event, readEvents)
import qualified File

readFile :: IO [Event]
readFile = do
  handleEvts <- foldr readEvents (Just []) . lines <$> File.getContent "store"
  handleEvtsV0 <- V0.handleEvts
  let evts = fromMaybe [] (handleEvts <|> handleEvtsV0)
  if null evts then return () else Store.writeFile evts
  return evts

writeFile :: [Event] -> IO ()
writeFile evts = writeFile' evts' =<< File.getPath "store"
  where
    evts' = unlines $ map show evts
    writeFile' = flip Prelude.writeFile

appendFile :: [Event] -> IO ()
appendFile = mapM_ writeEvent
  where
    writeEvent evt = appendToStore evt =<< File.getPath "store"
    appendToStore evt store = Prelude.appendFile store $ show evt ++ "\n"
