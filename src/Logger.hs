module Logger where

-- This module logs all the message to a channel to a file inside log
-- directory. Directory can be specified but will default to log in the root of
-- the project.

import System.IO
import Network.SimpleIRC
import Data.Time
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.Directory

getTimeStr :: IO String
getTimeStr = do
  time <- getCurrentTime
  return $ formatTime defaultTimeLocale " [%R %F] " time


createLogMessage :: IrcMessage -> IO String
createLogMessage m = do
  timeStr <- getTimeStr
  return $ concat [nick , timeStr , msg , "\n"]
      where
        nick = B.unpack $ fromJust $ mNick m
        msg = B.unpack $ mMsg m

writeLog :: IO FilePath -> IrcMessage -> IO ()
writeLog file m = do
  log <- createLogMessage m
  logFile <- file
  appendFile logFile log

defaultLogPath :: IO FilePath
defaultLogPath = do
  homeDir <- getHomeDirectory
  return (homeDir ++ "/hasbot.txt")
             
defaultLogger :: IrcMessage -> IO ()
defaultLogger = writeLog defaultLogPath
