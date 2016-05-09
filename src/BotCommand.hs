module BotCommand where

import Data.ByteString
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error


{-
The input can either be a command or a normal message.
If it is a command then it needs to be represented in a form where it is easier to add more operations without altering the processing-of-the-message part
A command takes 2 functions. One to transform the input string into required types and other the actual function that performs the operation.
-}

data Command = Command
 { execute   :: [String] -> String,
   args      :: [String]
 }
type CommandName = String
type CommandMap = CommandName -> Maybe ([String] -> Command)

type Message = String

data ChatMsg a = Msg String | Cmd Command

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

parseCmd :: GenParser Char st (ChatMsg a)
parseCmd = do
    doc <- chars
    eof
    return $ (doc) 
-- parse error for undefined command

chars :: GenParser Char st (ChatMsg a)
chars = do
    try 
     command <|> message

command :: GenParser Char st (ChatMsg a)
command = try $ do
	       esclamation <- oneOf "-"
	       command <- many alphaNum
	       argString <- getInput
	       case get command commands of
	       	Nothing -> fail (command ++ " not found")
	       	Just f  -> return $ Cmd (f (words argString))
	       


message :: GenParser Char st (ChatMsg a)
message = do
	       msg <- getInput
	       return (Msg msg)



------------custome commands------------------------------------------------------------------------


neg :: [String] -> Command
neg s = Command {execute = (\i -> show (-i)).convertToInt, args = s}

add :: [String] -> Command
add s = Command { execute = (\(i,j) -> show (i+j)).convertToIntInt, args = s}


convertToInt :: [String] -> Int
convertToInt [x] = read x :: Int
convertToInt _   = undefined --use the type cast monad??

convertToIntInt :: [String] -> (Int,Int)
convertToIntInt (x:y:[]) = let i = read x :: Int
                               j = read y :: Int
                           in (i,j)
convertToIntInt _        = undefined