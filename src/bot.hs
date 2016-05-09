--use -XBangPatterns while compiling

import Network.SimpleIRC hiding (Command,parse)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import BotCommand --TODO there is a command type in SimpleIRC. Check

--EventFunc = MIrc -> IrcMessage -> IO ()
onMessage :: EventFunc
onMessage s m = case commandParser msg of
                 Left error           -> putStrLn $ show error
                 Right (Msg onlyMsg) -> putStrLn $ show onlyMsg
                 Right (Cmd (cmd))  -> putStrLn $ show $ process cmd
                where msg = B.unpack $ mMsg m
{-onMessage s m
    | msg == toByteString "!Hi!" =  sendMsg s chan $ sendToSender "Hi there!"
    | B.isPrefixOf (toByteString "!") msg = sendMsg s sender $ sendToSender (execute msg)
    | otherwise = putStrLn $ show m
    where chan = fromJust $ mChan m
          sender = fromJust $ mNick m
          sender' = B.unpack $ fromJust $ mNick m
          msg = mMsg m
          toByteString = B.pack
          sendToSender m = toByteString $ sender' ++ ": " ++ m

-}

test :: String -> IO ()
test msg = case commandParser msg of
                 Left error           -> putStrLn $ show error
                 Right (Msg onlyMsg) -> putStrLn $ show onlyMsg
                 Right (Cmd (cmd))  -> putStrLn $ show $ process cmd


execute :: B.ByteString -> String
execute m = "Sorry, " ++ B.unpack m ++ "command not found!"

events = [(Privmsg onMessage)]

freenode = (mkDefaultConfig "irc.freenode.net" "hasbot")
           {cChannels = ["##maxking"]
           , cEvents = events
           }
--main = connect freenode False True
{-main = do
  line <- getLine
  putStrLn ("input " ++ line)
  test line
-}

process :: Command -> String
process (Command f ss) = f ss

commandParser :: String -> Either ParseError (ChatMsg a)
commandParser = parse parseCmd ""

