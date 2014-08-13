-- CIS 194 Homework 2

module Log where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage]) -> Int -> FilePath -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage]) -> ([LogMessage] -> [String]) -> FilePath -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file

parseMessage :: String -> LogMessage
parseMessage [] = Unknown "Empty"
parseMessage s = getLogMessage (words s)


getLogMessage::[String] -> LogMessage
getLogMessage [] = Unknown "Empty"
getLogMessage (x:y:z:xs) = case x of
                          "I" -> LogMessage Info (read y ::Int) (unwords (z:xs))
                          "E" -> LogMessage (Error (read y ::Int)) (read z ::Int) (unwords xs)
                          "W" -> LogMessage Warning (read y::Int) (unwords (z:xs))

parse :: String -> [LogMessage]
parse [] = []
parse s = map parseMessage (lines s)

data Comparison = GREATERTHAN
                  | LESSTHAN
                  | EQUAL

compare' :: Int -> Int -> Comparison
compare' x y
  | x < y = LESSTHAN
  | x > y = GREATERTHAN
  | otherwise = EQUAL

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown "Empty") mt = mt
insert lm Leaf  = Node Leaf lm Leaf
insert (LogMessage m t s) (Node l (LogMessage a b c) r) = case compare' t b of
                                           GREATERTHAN -> Node l (LogMessage a b c) (insert (LogMessage m t s) r)
                                           LESSTHAN -> Node (insert (LogMessage m t s) l) (LogMessage a b c) r

sortLogMessages :: [LogMessage] -> MessageTree
-- sortLogMessages [] = Leaf
-- sortLogMessages (x:xs) = insert x (sortLogMessages xs)
sortLogMessages = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (x:xs) = case x of
                        Unknown "Empty" -> whatWentWrong xs
                        LogMessage m n s -> case m of
                                              Error i -> case compare' i 50 of
                                                        GREATERTHAN -> s : whatWentWrong xs
                                                        LESSTHAN -> whatWentWrong xs
                                                        EQUAL -> whatWentWrong xs
                                              Info -> whatWentWrong xs
                                              Warning -> whatWentWrong xs


-- whatWentWrong  <$> (testParse parse 100 "sample.log")
-- inOrder <$> (sortLogMessages <$> (testParse parse 100 "error.log"))
