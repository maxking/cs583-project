
import Network.SimpleIRC hiding (Command,parse)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import BotCommand


-- | onMessage Event handler. According the RFC 2812 , PrivMsg is used to send
--  message to a user, a channel and a user in the channel.
--  type EventFunc = MIrc -> IrcMessage -> IO ()
onMessage :: EventFunc
onMessage s m = sendMsg s (fromJust $ mOrigin m) (process m)

-- | The list of event and their handlers. We only focus on responding to
--  messages which generate the PrivMsg event.
events = [(Privmsg onMessage)]

-- | The default configuration for the IRC server to join.
freenode = (mkDefaultConfig "irc.freenode.net" botName)

           {cChannels = ["##maxking"],
           cEvents = events}

-- | The main event loop
-- connect :: MIrc -> Bool (threaded) -> Bool (Debug Mode) -> IO (Either IOError MIrc)
main = connect freenode False True

-- | Process a command and return a response
runCommand :: Command -> [String] -> String
runCommand (Command f g u) = g.f 

-- | Parse the message which contains either a command starting with ! or anything 
commandParser :: String -> Either ParseError (ChatMsg)
commandParser = parse parseCmd ""

-- | Process a message and return a response
-- TODO wrap the result in IO and use try catch block here for run time exceptions.
--
process :: IrcMessage -> B.ByteString
process m = case commandParser (stripBotName msg) of
                 Left error           -> B.pack $ show error
                 Right (Msg onlyMsg)  -> B.pack onlyMsg
                 Right (Cmd cmd args) -> B.pack $ runCommand cmd args
            where msg = B.unpack $ mMsg m

stripBotName :: String -> String
stripBotName s  = case stripPrefix (botName++":") (stripSpaces s) of
                   Just x  -> stripSpaces x
                   Nothing -> stripSpaces s

stripSpaces :: String -> String
stripSpaces  = T.unpack . T.strip . T.pack

--Test function instead of passing the entire IRCMessage
-- test "hi"
-- test "!neg 3"
-- test "!add 3 4"
test :: String -> IO ()
test msg = do case commandParser (msg) of
                 Left error           -> putStrLn $ show error
                 Right (Msg onlyMsg)  -> putStrLn onlyMsg
                 Right (Cmd cmd args) -> putStrLn $ runCommand cmd args


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
