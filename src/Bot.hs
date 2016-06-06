module Bot where

import Network.SimpleIRC hiding (Command,parse)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import BotCommand
import Logger
import Config
import Categorize
import Control.Exception

-- | onMessage Event handler. According the RFC 2812 , PrivMsg is used to send
--  message to a user, a channel and a user in the channel.
--  type EventFunc = MIrc -> IrcMessage -> IO ()
onMessage :: EventFunc
onMessage s m = case categorize m of
                  ( _, False)              -> defaultLogger m
                  (ChanMsg, True)          -> reply s m False
                  (ChanMsgtoBot, True)     -> reply s m True
                  (PrivMsg, True)          -> reply s m False

reply :: MIrc -> IrcMessage -> Bool -> IO ()
reply s m b = (process b $ mMsg m) >>= sendMsg s (fromJust $ mOrigin m)

-- | Process a message and return a response
--
process :: Bool -> B.ByteString -> IO B.ByteString
process True  b = (process2 . stripBotName . B.unpack) b >>= return . B.pack
process False b = (process2 . B.unpack) b >>= return . B.pack

process2 :: String -> IO String
process2 m = case commandParser m of
                 Left error           -> return $ show error
                 Right (Msg onlyMsg)  -> return onlyMsg
                 Right (Cmd cmd args) -> runCommand cmd args

-- | Parse the message which contains either a command starting with ! or anything
commandParser :: String -> Either ParseError (ChatMsg)
commandParser = parse parseCmd ""

-- | Process a command and return a response
runCommand :: Command -> [String] -> IO String
runCommand (Command f g u) args = do
  res <- evaluate (f args) `catch` excepHandler
  case res of
   Left InvalidNumber -> return (showNumberError ++ u)
   Left InvalidType   -> return (showTypeError ++ u)
   Right arg          -> return (g arg)

excepHandler :: SomeException -> IO (Either ArgError a)
excepHandler _ = return (Left InvalidType)

-------------------------Tests--------------------------------------------------

-- | Test function without the IRC communication
-- >>>test "hi"
-- hi
-- >>>test "!hi"
-- Hello!
-- >>>test "!neg 3"
-- -3
-- >>>test "!add 3 4"
-- 7
test :: String -> IO ()
test msg = do case commandParser (msg) of
                 Left error           -> putStrLn $ show error
                 Right (Msg onlyMsg)  -> putStrLn onlyMsg
                 Right (Cmd cmd args) -> runCommand cmd args >>= putStrLn


-- | 1. Define a test IRC message to the channel for testing
msg :: IrcMessage
msg = IrcMessage {mNick = Just $ B.pack "maxking",
                  mUser = Just $ B.pack "~phoenix",
                  mHost = Just $ B.pack "104.131.128.74",
                  mServer = Nothing,
                  mCode = B.pack "PRIVMSG",
                  mMsg = B.pack "hi",
                  mChan = Just $ B.pack "##maxking",
                  mOrigin = Just $ B.pack "##maxking",
                  mOther = Nothing,
                  mRaw = B.pack ":maxking!~phoenix@104.131.128.74 PRIVMSG ##maxking :hi"}

-- | 2. A test IRC message to channel highlighting the bot
msg2 :: IrcMessage
msg2 = IrcMessage {mNick = Just $ B.pack "maxking",
                   mUser = Just $ B.pack "~phoenix",
                   mHost = Just $ B.pack "104.131.128.74",
                   mServer = Nothing,
                   mCode = B.pack "PRIVMSG",
                   mMsg = B.pack "hasbot: How you you man?",
                   mChan = Just $ B.pack "##maxking",
                   mOrigin = Just $ B.pack "##maxking",
                   mOther = Nothing,
                   mRaw = B.pack ":maxking!~phoenix@104.131.128.74 PRIVMSG ##maxking :hasbot: How you you man?"}

-- | 3. A test IRC message to bot as private message
msg3 :: IrcMessage
msg3 = IrcMessage {mNick = Just $ B.pack "maxking",
                   mUser = Just $ B.pack "~phoenix",
                   mHost = Just $ B.pack "104.131.128.74",
                   mServer = Nothing,
                   mCode = B.pack "PRIVMSG",
                   mMsg = B.pack "Hi Man!",
                   mChan = Just $ B.pack "hasbot",
                   mOrigin = Just $ B.pack "maxking",
                   mOther = Nothing,
                   mRaw = B.pack ":maxking!~phoenix@104.131.128.74 PRIVMSG hasbot :Hi Man!"}
