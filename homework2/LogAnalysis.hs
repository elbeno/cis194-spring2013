{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- exercise 1: parsing log messages

-- (inadequate error handling here)

parseMessage :: String -> LogMessage
parseMessage s = parseMessage' $ words s
  where parseMessage' (w:ws) = case w of
          "E" -> parseError ws
          "W" -> parseWarning ws
          "I" -> parseInfo ws
          _ -> Unknown s

parseNonError :: MessageType -> [String] -> LogMessage
parseNonError m (ts:rest) = LogMessage m t (unwords rest)
  where t = read ts

parseError :: [String] -> LogMessage
parseError (e:rest) = parseNonError (Error $ read e) rest

parseWarning :: [String] -> LogMessage
parseWarning = parseNonError Warning

parseInfo :: [String] -> LogMessage
parseInfo = parseNonError Info

parse ::String -> [LogMessage]
parse = map parseMessage . lines

-- exercise 2: sorting log lines

-- incomplete pattern match is OK because Unknowns don't get put in the tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ta _) (Node l m'@(LogMessage _ tb _) r)
  | ta <= tb = Node (insert m l) m' r
  | otherwise = Node l m' (insert m r)

-- exercise 3: building a tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- exercise 4: in-order traversal

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- exercise 5: relevance

severityOver50 :: LogMessage -> Bool
severityOver50 (LogMessage (Error e) _ _) = e >= 50
severityOver50 _ = False

-- incomplete pattern match is OK because non-errors are filtered out
extractMsg :: LogMessage -> String
extractMsg (LogMessage _ _ s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map extractMsg . filter severityOver50 . inOrder . build $ ms

-- exercise 6: the culprit

culprit :: IO String
culprit = do
  errs <- testWhatWentWrong parse whatWentWrong "error.log"
  return $ map head errs
