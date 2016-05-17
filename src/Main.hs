import Bot
import Config
import Network.SimpleIRC hiding (Command, parse)
import Webserver
import Control.Concurrent
import Network.Wai.Handler.Warp

-- | The list of event and their handlers. We only focus on responding to
--  messages which generate the PrivMsg event.
events = [(Privmsg onMessage)]

-- | The default configuration for the IRC server to join.
freenode = (mkDefaultConfig "irc.freenode.net" Config.botName)

           {cChannels = ["##maxking"],
           cEvents = events}

-- | The main event loop
-- connect :: MIrc -> Bool (threaded) -> Bool (Debug Mode) -> IO (Either IOError MIrc)
main = do
  putStrLn $ "Listening on http://localhost:" ++ show Config.port
  forkIO $ run Config.port hasbotWebApp
  connect freenode False True
