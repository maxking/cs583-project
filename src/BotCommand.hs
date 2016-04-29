module BotCommand where

import Data.ByteString


{-
The input can either be a command or a normal message.
If it is a command then it needs to be represented in a form where it is easier to add more operations without altering the processing-of-the-message part
A command takes 2 functions. One to transform the input string into required types and other the actual function that performs the operation.
-}

data Command a = Cmd ([String] -> a) (a -> String) [String]

type Message = String

data ChatMsg a = Either Message (Command a)

neg :: [String] -> Command Int
neg = Cmd convertToInt (\i -> show (-i))

add :: [String] -> Command (Int,Int)
add = Cmd convertToIntInt (\(i,j) -> show (i+j))


convertToInt :: [String] -> Int
convertToInt [x] = read x :: Int
convertToInt _   = undefined --use the type cast monad??

convertToIntInt :: [String] -> (Int,Int)
convertToIntInt (x:y:[]) = let i = read x :: Int
                               j = read y :: Int
                           in (i,j)
convertToIntInt _        = undefined