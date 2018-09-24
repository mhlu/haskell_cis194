--[># OPTIONS_GHC -Wall #<]
module LogAnalysis where

import Log
import Text.Read

{- data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq) -}

-- ex 1
isInt :: String -> Bool
isInt n = case ( readMaybe n :: Maybe Int ) of
            Nothing -> False
            _ -> True

parseMessage :: String -> LogMessage
parseMessage = helper . words
    where helper ("I":time:msg) | isInt time = LogMessage Info (read time) (unwords msg)
          helper ("W":time:msg) | isInt time = LogMessage Warning (read time) (unwords msg)
          helper ("E":level:time:msg) | isInt level && isInt time = LogMessage (Error (read level)) (read time) (unwords msg)
          helper ss = Unknown (unwords ss)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

{- data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq) -}

-- ex 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time _ ) (Node leftSubTree node@(LogMessage _ nodeTime _ ) rightSubTree)
    | time < nodeTime = Node (insert msg leftSubTree) node rightSubTree
    | otherwise = Node leftSubTree node (insert msg rightSubTree)

-- ex 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- ex 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- ex 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map errorToString . filter isWrong
    where errorToString (LogMessage _ _ msg) = msg
          isWrong (LogMessage (Error level) _ _ ) | level >= 50 = True
          isWrong _ = False
