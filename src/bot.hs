import Network.SimpleIRC hiding (Command)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import BotCommand --TODO there is a command type in SimpleIRC. Check

-- | onMessage Event handler. According the RFC 2812 , PrivMsg is used to send
--  message to a user, a channel and a user in the channel.
onMessage :: EventFunc
onMessage s m = undefined


execute :: B.ByteString -> String
execute m = "Sorry, " ++ B.unpack m ++ "command not found!"

events = [(Privmsg onMessage)]

freenode = (mkDefaultConfig "irc.freenode.net" "hasbot")
           {cChannels = ["##maxking"],
           cEvents = events}

main = connect freenode False True


command :: Command a -> String
command (Cmd c f ss) = f (c ss)

-- may be we can define a tiny weeny parser (using the parser combinator) to
-- parse the input text into ChatMsg type?
-- parse also defined in simpleIRC
-- parse :: String -> ChatMsg a
-- parse = undefined
-- parse error for undefined command



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
