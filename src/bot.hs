import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

onMessage :: EventFunc
onMessage s m
    | msg == toByteString "!Hi!" =  sendMsg s chan $ sendToSender "Hi there!"
    | B.isPrefixOf (toByteString "!") msg = sendMsg s sender $ sendToSender (execute msg)
    | otherwise = putStrLn $ show m
    where chan = fromJust $ mChan m
          sender = fromJust $ mNick m
          msg = mMsg m
          toByteString = B.pack
          sendToSender m = toByteString $ sender ++ ": " ++ m

                           

execute :: B.ByteString -> String
execute m = "Sorry, " ++ B.unpack m ++ "command not found!"

events = [(Privmsg onMessage)]

freenode = (mkDefaultConfig "irc.freenode.net" "hasbot")
           {cChannels = ["##maxking"]
           , cEvents = events
           }
main = connect freenode False True
