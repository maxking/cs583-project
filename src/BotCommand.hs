module BotCommand where

import Prelude hiding (mempty)
import Data.ByteString
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error


{-
The input can either be a command or a normal message.
If it is a command then it needs to be represented in a form where it is easier 
to add more operations without altering the processing-of-the-message part
A command takes 2 functions. One to transform the input string into required types 
and other the actual function that performs the operation.
Update: With the above approach, all the functions needed to be of same type ie., a had to be a common type. 
This is cannot be true because some commands may take different types of arguments.
Therefore, removed the intermediate type. 
-}
--TODO add usage
data Command = Command
 { execute   :: [String] -> String,
   args      :: [String],
   usage     :: String
 }

type CommandName = String
type CommandMap = CommandName -> Maybe ([String] -> Command)

type Message = String

--add sequence operator ">>="
data ChatMsg = Msg String | Cmd Command

------command getter and setter-------------------------------------------------------------

mempty :: CommandMap
mempty = \_ -> Nothing

set :: CommandName -> ([String] -> Command) -> CommandMap -> CommandMap
set x i env = \y -> if x == y then (Just i) else env y

get :: CommandName -> CommandMap -> Maybe ([String] -> Command)
get x env = env x

--Set Commands
--TODO should we think of some otherway to map command name to command type??
commands :: CommandMap
commands = set "add" add (set "neg" neg mempty)

------Parser--------------------------------------------------------------------------------

parseCmd :: GenParser Char st (ChatMsg)
parseCmd = do
    doc <- chars
    eof
    return $ (doc) 
-- parse error for undefined command

--TODO parse "hasbot: !Cmd", "hasbot: message"
chars :: GenParser Char st (ChatMsg)
chars = do
    try processCommand <|> message

--TODO need a way to specify error due to incorrect number of arguments
processCommand :: GenParser Char st (ChatMsg)
processCommand = try $ do
               char '!' 
               command <- many (noneOf " ")
               argString <- many anyChar
               case get command commands of
                Nothing -> fail (command ++ " not found")
                Just f  -> return $ Cmd (f (words argString))

--For now anything apart from commands is printed as it is.
message :: GenParser Char st (ChatMsg)
message = do
           msg <- many anyChar
           return (Msg msg)



------------custome commands---------------------------------------------------


neg :: [String] -> Command
neg s = Command {execute = (\i -> show (-i)).convertToInt, args = s, usage = "Usage: !neg <integer>"}

add :: [String] -> Command
add s = Command { execute = (\(i,j) -> show (i+j)).convertToIntInt, args = s, usage = "Usage: !add <integer> <integer>"}


convertToInt :: [String] -> Int
convertToInt []  = 1
convertToInt (x:xs) = read x :: Int

convertToIntInt :: [String] -> (Int,Int)
convertToIntInt (x:y:[]) = let i = read x :: Int
                               j = read y :: Int
                           in (i,j)
convertToIntInt _        = undefined
