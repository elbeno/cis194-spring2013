{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

-- exercise 1: tools for working with GuestLists

-- add an Employee to a GuestList
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

-- GuestList is a Monoid
instance Monoid GuestList where
  mempty = GL [] 0
  (GL es f) `mappend` (GL es' f') = GL (es ++ es') (f + f')

-- which of two GuestLists has more fun?
-- GuestList is an instance of Ord already...
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- exercise 2

-- a fold algorithm for rose trees
treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f l (Node a ts) = f a l'
  where l' = map (treeFold f l) ts

-- exercise 3

-- the best GuestList with the current boss is the boss plus the total GuestList
-- without the sub-bosses

-- the best GuestList without the current boss is the total best GuestList
-- (which could be made from GuestLists with or without the sub-bosses)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (bestWith, bestWithout)
  where bestWith = glCons b $ mconcat $ map snd gls
        bestWithout = mconcat $ map (uncurry moreFun) gls

-- exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel []

-- exercise 5

formatGuestList :: GuestList -> [String]
formatGuestList (GL emps fun) = ("Total fun: " ++ show fun) : sort (map empName emps)

main :: IO ()
main = do
  company <- readFile "company.txt"
  let party = maxFun (read company :: Tree Employee)
  mapM_ putStrLn (formatGuestList party)
