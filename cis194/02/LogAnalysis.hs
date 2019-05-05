{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- ex1: parse individual message --
parseMessage :: String -> LogMessage
parseMessage ('I':' ':xs) = LogMessage Info (parseInt $ head $ words xs) (unwords $ tail $ words xs)
parseMessage ('W':' ':xs) = LogMessage Warning (parseInt $ head $ words xs) (unwords $ tail $ words xs)
parseMessage ('E':' ':xs) = LogMessage (Error (parseInt $ head $ words xs)) (parseInt $ head $ tail $ words xs) (unwords $ tail $ tail $ words xs) 
parseMessage s = Unknown s

-- Parse the entire log
-- parses an entire log file at once and returns its contents as a list of LogMessages
parse :: String -> [LogMessage]
parse s = parseLines (lines s);

-- helper parse lines
parseLines :: [String] -> [LogMessage]
parseLines [] = [];
parseLines (s:xs) = parseMessage s : parseLines(xs); 

-- parseInt
parseInt :: String -> Int
parseInt s = read s

-- insert to tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _, t) = t;
insert (LogMessage msgType ts txt) = 
