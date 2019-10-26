module Store where

import           Control.Exception
import           System.IO.Error
import           System.Directory

import           Event
import           State
import           Utils

readEvents :: IO [Event]
readEvents = do
  let tmpStorePath = "/tmp/unfog-store"
  storePath <- getFilePath "store"
  copyFile storePath tmpStorePath `catch` catchCopyErrors
  storeContent <- readFile tmpStorePath `catch` catchReadErrors
  removeFile tmpStorePath `catch` catchRemoveErrors
  return $ map read $ lines storeContent
 where
  catchCopyErrors err | isDoesNotExistError err = return ()
                      | otherwise               = throwIO err
  catchReadErrors err | isDoesNotExistError err = return ""
                      | otherwise               = throwIO err
  catchRemoveErrors err | isDoesNotExistError err = return ()
                        | otherwise               = throwIO err

writeEvent :: Event -> IO ()
writeEvent event = getFilePath "store" >>= flip appendFile (show event ++ "\n")
