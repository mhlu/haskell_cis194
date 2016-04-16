{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

isInt :: String -> Bool
isInt n = case reads n :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False

parseMessage:: String -> LogMessage
parseMessage msg = par . splitAt 2 . words $ msg
    where par (["E", n], str) | isInt n && (isInt . head $ str) = LogMessage (Error (read n::Int)) (read (head str)::Int) (unwords $ tail str)
          par (["W", n], str) | isInt n = LogMessage Warning (read n::Int) (unwords str)
          par (["I", n], str) | isInt n = LogMessage Info (read n::Int) (unwords str)
          par _ = Unknown msg

parse:: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t1 _) (Node lhs msg rhs)
    | t1 < t2 = Node (insert m lhs) msg rhs
    | t1 > t2 = Node lhs msg (insert m rhs)
    | otherwise = Node lhs m rhs
    where (LogMessage _ t2 _) = msg

build :: [LogMessage] -> MessageTree
build = foldl (\acc n -> insert n acc) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lhs m rhs) = (inOrder lhs) ++ [m] ++ (inOrder rhs)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ msg) -> msg) . filter filterHelper . sortMsg
    where sortMsg = inOrder . build
          filterHelper (LogMessage (Error n) _ _) | n >= 50 = True
          filterHelper _ = False
