module Categorize where

import Config
import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B
-- This module has a very simple function to categorize the messages into one of
-- the three types below:
-- 1. Message to channel (chanMsg) - they are just logged to a file
-- These can be identified by same mOrigin and mChan attributes
--
-- 2. Message to channel but starting with the nick (chanMsgtoBot) - they are logged and also
--    processed to return the response to the user.
-- These can be idenfied as 1 with mMsg string with nick
--
-- 3. Private Message to the channel (privMsg)
-- These can be identified with same mNick and mOrigin attributes

data IrcMsgType = ChanMsg | ChanMsgtoBot | PrivMsg
                deriving (Show, Eq)

categorize :: IrcMessage -> IrcMsgType
categorize m = case mOrigin m == mChan m of
                 True -> case isMsgtoBot m of
                           True -> ChanMsgtoBot
                           False -> ChanMsg
                 False -> case mOrigin m == mNick m of
                            True  -> PrivMsg
                            False -> undefined

isMsgtoBot :: IrcMessage -> Bool
isMsgtoBot m = B.isPrefixOf (B.pack Config.botName) (mMsg m)
