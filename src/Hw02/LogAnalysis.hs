module LogAnalysis where

import Log


parseMessageWords :: [String] -> LogMessage
parseMessageWords ("I":time:message) = LogMessage Info (read time) (unwords message)
parseMessageWords ("W":time:message) = LogMessage Warning (read time) (unwords message)
parseMessageWords ("E":code:time:message) = LogMessage (Error (read code)) (read time) (unwords message)
parseMessageWords message = Unknown (unwords message)

parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines