{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


-- exercise 1: appending JoinLists

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ l = l
l +++ Empty = l
l1 +++ l2 = Append (tag l1 `mappend` tag l2) l1 l2

-- exercise 2: fast indexing

-- test functions

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

listToJl :: [a] -> JoinList Size a
listToJl = foldl (+++) Empty . map (Single (Size 1))

-- indexJ: retrieve the indexed element from a JoinList

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ a)
  | n == 0 = Just a
  | otherwise = Nothing
indexJ n (Append _ l1 l2)
  | n < t = indexJ n l1
  | otherwise = indexJ (n-t) l2
  where t = getSize $ size $ tag l1

-- dropJ: drop the first n elements from a JoinList

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 l = l
dropJ n (Append _ l1 l2)
  | n >= t = dropJ (n-t) l2
  | otherwise = Append m l1' l2
  where t = getSize $ size $ tag l1
        l1' = dropJ n l1
        m = tag l1' `mappend` tag l2
dropJ _ _  = Empty

-- takeJ: take the first n elements from a JoinList

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n (Append _ l1 l2)
  | n == t = l1
  | n < t = takeJ n l1
  | otherwise = Append m l1 l2'
  where t = getSize $ size $ tag l1
        l2' = takeJ (n-t) l2
        m = tag l1 `mappend` tag l2'
takeJ _ l = l

-- exercise 3: scrabble scoring

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- exercise 4: combining monoids

-- a naive fold implementation of fromString forms a degenerate tree, so we
-- should balance it

balanceJ :: JoinList (Score, Size) String -> JoinList (Score, Size) String
balanceJ l@(Append (_,s) _ _) = l1 +++ l2
  where n = getSize s `div` 2
        l1 = balanceJ $ takeJ n l
        l2 = balanceJ $ dropJ n l
balanceJ l = l

-- instance of Buffer using our JoinList

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l1 l2) = toString l1 ++ "\n" ++ toString l2

  fromString = balanceJ . foldl (+++) Empty .
               map (\s -> Single (scoreString s, Size 1) s) . lines

  line = indexJ

  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n+1) jl

  numLines Empty = 0
  numLines (Single _ _) = 1
  numLines (Append (_,n) _ _) = getSize n

  value Empty = 0
  value (Single (s,_) _) = getScore s
  value (Append (s,_) _ _) = getScore s

fromStringList :: [String] -> JoinList (Score, Size) String
fromStringList = foldl (+++) Empty . map fromString

main :: IO ()
main = runEditor editor $ fromStringList
       [ "This buffer is for notes you don't want to save, and for"
       , "evaluation of steam valve coefficients."
       , "To load a different file, type the character L followed"
       , "by the name of the file."
       ]
