{-# LANGUAGE OverloadedStrings #-}
module JSON where

import           Data.Aeson

data ResponseJSON =
  ResponseJSON { _success :: Bool , _data :: String }

instance ToJSON ResponseJSON where
  toJSON (ResponseJSON _success _data) =
    object ["success" .= if _success then 1 else 0 :: Int, "data" .= _data]

