{-# LANGUAGE ExistentialQuantification #-}
module BotCommand where

import Prelude hiding (mempty)
import Data.ByteString
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Control.Monad.Except

botName = "hasbot"

{-
Hint
The input can either be a command or a normal message.
If it is a command then it needs to be represented in a form where it is easier 
to add more operations without altering the processing-of-the-message part
A command takes 2 functions. One to transform the input string into required types 
and other the actual function that performs the operation.
A command takes a String that contains the usage information.
Update: With the above approach, all the functions needed to be of same type ie., a had to be a common type. 
This is cannot be true because some commands may take different types of arguments.
Therefore, removed the intermediate type.
Update2: Added the intermediate type back. This is possible with a language extension called extensible types. 
-}
type ErrMsg = String
data Command = forall a. Command ([String] -> Either ArgError a) (a -> String) String

type CommandName = String
type CommandMap = [(CommandName,Command)]


--add sequence operator ":>:"
data ChatMsg = Msg String | Cmd Command [String]


-----Error types
data ArgError = InvalidNumber | InvalidType

------command getter and setter-------------------------------------------------------------

set :: CommandName -> Command -> CommandMap -> CommandMap
set x i cs = (x,i) : cs

get :: CommandName -> CommandMap -> Maybe (Command)
get x []         = Nothing
get x ((n,c):cs) 
   |x == n       = Just c
   |otherwise    = get x cs

------Parser--------------------------------------------------------------------------------

parseCmd :: GenParser Char st (ChatMsg)
parseCmd = do
    doc <- isCommand
    eof
    return $ (doc) 
-- parse error for undefined command

chars :: GenParser Char st (ChatMsg)
chars = do 
    try processCommand <|> message

--processCommands :: GenParser Char st (ChatMsg)
--processCommands = try $ do 
--	             commands <- many processCommand

--parseBotName :: GenParser Char st (String)
--parseBotName = do 
--	           string (botName ++ ":")

--TODO need a way to specify error due to incorrect number of arguments
isCommand :: GenParser Char st (ChatMsg)
isCommand = do 
        (char '!' >> processCommand) <|> message

processCommand :: GenParser Char st (ChatMsg)
processCommand = try $ do 
               command <- many (noneOf " ")
               argString <- many anyChar
               case get command commands of
                Nothing -> fail (show (command) ++ " not found")
                Just f  -> return $ Cmd f (words argString)

--For now anything apart from commands is printed as it is.
message :: GenParser Char st (ChatMsg)
message = do
           msg <- many anyChar
           return (Msg msg)

-----Error messages
showTypeError = "Invalid type of arguments.\n"
showNumberError = "Invalid number of arguments.\n"
-----------------------------------------------------------------------------
--List of all the custom commands

commands :: CommandMap
commands = ("hi", hi) : ("add", add) : ("neg", neg): []


------------custom commands---------------------------------------------------

hi :: Command
hi = Command (\_ -> Right ()) (\_ -> "Hello!") "Usage: !hi"

neg :: Command
neg = Command convertToInt (show.negate) "Usage: !neg <integer>"

add :: Command
add = Command convertToIntInt (\(i,j) -> show (i+j)) "Usage: !add <integer> <integer>"

{- The conversions need to be strict so that errors can be caught
 to show proper messages.
 The value of seq a b is bottom if a is bottom, and otherwise equal to b. 
 seq is usually introduced to improve performance by avoiding unneeded laziness. 
 Both its arguments are strict 
-}
convertToInt :: [String] -> Either ArgError Int
convertToInt [x] = let i = read x :: Int in i `seq` return i
convertToInt _   = throwError InvalidNumber

convertToIntInt :: [String] -> Either ArgError (Int,Int)
convertToIntInt (x:y:[]) = let i = (read x :: Int) 
                               j = (read y :: Int)
                           in i `seq` j `seq` return (i,j)
convertToIntInt _        = throwError InvalidNumber
