{-# LANGUAGE OverloadedStrings #-}
module Webserver where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
--import qualified Data.ByteString.Char8 as B
--import Data.Monoid
import Logger

-- main = do
--     let port = 3000
--     putStrLn $ "Listening on port " ++ show port
--     run port app

hasbotWebApp req respond = do
  logs <- defaultLogReader
  respond $ returnLog logs

returnLog logs = responseBuilder status200 [ ("Content-Type", "text/plain") ]
                 $ mconcat $ map copyByteString [logs]
