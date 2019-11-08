module Utils where

import           Prelude                 hiding ( (<>) )
import           Data.List
import           Data.Maybe
import           System.Environment
import           Text.PrettyPrint.Boxes
import           Data.Aeson              hiding ( Error )
import qualified Data.ByteString.Lazy.Char8    as BL

import           DataType
import           JSON

getFilePath :: String -> IO String
getFilePath file = (++ "/" ++ file) <$> getConfigDirPath

getConfigDirPath :: IO String
getConfigDirPath = lookupEnv "XDG_CONFIG_HOME" >>= withDefault
 where
  withDefault maybePath = case maybePath of
    Just path -> (++ "/unfog") <$> return path
    Nothing   -> (++ "/.config/unfog") . fromMaybe "/tmp" <$> lookupEnv "HOME"

getDataType :: [String] -> DataType
getDataType args | "--json" `elem` args = JSON
                 | otherwise            = Text

startsByPlus :: String -> Bool
startsByPlus "+"       = False
startsByPlus ('+' : _) = True
startsByPlus _         = False

putJSON :: Bool -> String -> IO ()
putJSON _success _data = BL.putStr $ encode $ ResponseJSON _success _data

elog :: DataType -> String -> String -> IO ()
elog dataType cmd msg =
  let str = if null cmd then msg else cmd ++ ": " ++ msg
  in  case dataType of
        JSON -> putJSON False str
        Text -> putStrLn $ "\x1b[31munfog: " ++ str ++ "\x1b[0m"

-- Source:
-- https://codereview.stackexchange.com/questions/171992/pretty-printed-tables-in-haskell
table :: [[String]] -> Box
table rows =
  vsep <> hcat top (intersperse vsep (map formatColumn columns)) <> vsep
 where
  columns = transpose rows
  nrows   = length rows
  vsep    = vcat left $ map char ("+" ++ concat (replicate nrows "|+"))
  formatColumn items =
    hsep // vcat left (intersperse hsep (map (text . pad width) items)) // hsep
   where
    width = maximum $ map length items
    hsep  = text (replicate width '-')
    pad width x = x ++ replicate (width - length x) ' '
