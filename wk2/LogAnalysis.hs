{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case words str of
  ("I" : ts : msg) -> (LogMessage Info (read ts) (unwords msg))
  ("W" : ts : msg) -> (LogMessage Warning (read ts) (unwords msg))
  ("E" : se : ts : msg) -> (LogMessage (Error (read se)) (read ts) (unwords msg))
  _ -> Unknown str

parse :: String -> [LogMessage]
parse file = case lines file of
  (l : ls) -> parseMessage l : parse (unlines ls)
  [] -> []

getTime :: LogMessage -> TimeStamp
getTime (LogMessage (Error _) ts _) = ts
getTime (LogMessage _ ts _) = ts
getTime _ = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mst = mst
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left nmsg right)
  | getTime msg < getTime nmsg = Node (insert msg left) nmsg right
  | otherwise = Node left nmsg (insert msg right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error se) _ msg) : xs)
  | se >= 50 = msg : whatWentWrong xs
  | otherwise = whatWentWrong xs
whatWentWrong (_ : xs) = whatWentWrong xs
