module Categorize where

import Config
import Data.List
import Network.SimpleIRC
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
--import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Char
-- This module has a very simple function to categorize the messages into one of
-- the three types below:
-- 1. Message to channel (chanMsg) - they are just logged to a file
-- These can be identified by same mOrigin and mChan attributes
--
-- 2. Message to channel but starting with the nick (chanMsgtoBot) - they are
--    logged and also processed to return the response to the user.  These can
--    be idenfied as 1 with mMsg string with nick
--
-- 3. Private Message to the channel (privMsg)
-- These can be identified with same mNick and mOrigin attributes

data IrcMsgType = ChanMsg | ChanMsgtoBot | PrivMsg
                deriving (Show, Eq)

-- | Categorize the message in of the IrcMsgType and and check if it is is a
--   command i.e. starts with a !
categorize :: IrcMessage -> (IrcMsgType, Bool)
categorize m = case mOrigin m == mChan m of
                 True -> case isMsgtoBot m of
                           True  -> (ChanMsgtoBot, isCommand True m)
                           False -> (ChanMsg, isCommand False m)
                 False -> case mOrigin m == mNick m of
                            True  -> (PrivMsg, isCommand False m)
                            False -> undefined

isMsgtoBot :: IrcMessage -> Bool
isMsgtoBot m = B.isPrefixOf (B.pack Config.botName) (mMsg m)

runStrProcess :: (String -> String) -> B.ByteString -> B.ByteString
runStrProcess f = B.pack . f . B.unpack

isCommand :: Bool -> IrcMessage -> Bool
isCommand True  = isCommand1 . (runStrProcess stripBotName) . mMsg
isCommand False = isCommand1 . mMsg

isCommand1 :: B.ByteString -> Bool
isCommand1 = B.isPrefixOf (B.pack "!")

stripSpaces :: String -> String
stripSpaces  = T.unpack . T.strip . T.pack

stripBotName :: String -> String
stripBotName s  = case stripPrefix (Config.botName++":") (stripSpaces s) of
                   Just x  -> stripSpaces x
                   Nothing -> stripSpaces s
