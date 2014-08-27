module Golf where

import Control.Applicative
import Data.List

-- Code golf: short solutions to exercises. I've made no real attempt to make
-- the code minimal other than by style & prelude usage (e.g. the auxiliary
-- functions are sensibly-named, not single-letters).

-- exercise 1: hopscotch

-- First, everyNth will, when given an Int and a list, give us back a list
-- containing every nth value.

-- 1. zip the list with the cycling natural numbers up to n to get a list of pairs
-- 2. filter that list to the pairs whose naturals are divisible by n
-- 3. extract the second element from each pair to recover the original list elements

everyNth :: Int -> [a] -> [a]
everyNth n = map snd . filter ((==n) . fst) . zip (cycle [1..n])

-- Now we can write skips:

-- First, map everyNth over a list of Ints up to the length of the input: this
-- gives us a list of functions to apply to the input.

-- Then remember that list is an applicative functor, so we can use <*> to apply
-- a list of functions to a list of arguments. Every function is applied to
-- every argument, but in this case we just have a single argument: the input.

skips :: [a] -> [[a]]
skips l = map everyNth [1 .. length l] <*> [l]

-- exercise 2: local maxima

-- Simply check if an element is greater than the elements each side of it. This
-- is pretty short already.

-- Other ideas: maybe zip the list with itself somehow and filter?
-- Some sort of fold?

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:r)
  | y > x && y > z = y : localMaxima (z:r)
  | otherwise = localMaxima (y:z:r)
localMaxima _ = []

-- exercise 3: histogram

-- Find the frequency of each digit: sort the list, group it, and count the
-- frequency of each number. Make sure there's at least one of each, then
-- adjust, to get zeroes for missing numbers.
-- Note: haskell syntax doesn't permit (-1) as a section. Hence subtract.

frequencies :: [Integer] -> [Int]
frequencies = map (subtract 1 . length) . group . sort . (++ [0..9])

-- Convert a number to a column of stars, given the max m. Simply (m-n) spaces
-- and n stars, then the baseline.

numToColumn :: Int -> Int -> String
numToColumn m n = replicate (m-n) ' ' ++ replicate n '*' ++ "="

-- Render the columns: find the max height, so that we have the
-- (partially-applied) numToColumn with the right height. Then map that over the
-- frequency list to get the columns. Finally, transpose to get the graph the
-- right way up.

renderCols :: [Int] -> [String]
renderCols freqs = transpose $ map (numToColumn $ maximum freqs) freqs

-- The histogram itself: find the frequencies, and then render them as columns.
-- Join the lines together and add the legend.

histogram :: [Integer] -> String
histogram = (++ "0123456789\n") . unlines . renderCols . frequencies
