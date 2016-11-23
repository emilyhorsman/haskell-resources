{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage line =
    case words line of
        ("I":timestamp:remainder) ->
            LogMessage Info (read timestamp) (unwords remainder)
        ("W":timestamp:remainder) ->
            LogMessage Warning (read timestamp) (unwords remainder)
        ("E":severity:timestamp:remainder) ->
            LogMessage (Error (read severity)) (read timestamp) (unwords remainder)
        _ ->
            Unknown line

parse :: String -> [LogMessage]
parse log =
    map parseMessage (lines log)
