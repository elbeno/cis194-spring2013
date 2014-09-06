{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- exercise 1: naive fibs

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2: more efficient

fibs2 :: [Integer]
fibs2 = 0 : 1: zipWith (+) fibs2 (tail fibs2)

-- exercise 3: streams

data Stream a = Cons a (Stream a)

-- convert a stream to an infinite list

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

-- show for streams shows the first 20 elements

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- exercise 4: simple tools

-- repeat a value forever
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

-- map a function on to a stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

-- make a stream from a seed and an iterative function
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- exercise 5: some example streams

-- the natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- the "ruler" function: first, interleave two streams
interleave :: Stream a -> Stream a -> Stream a
interleave (Cons a as) b = Cons a (interleave b as)

-- now we can create the ruler by interleaving recursively
ruler :: Stream Integer
ruler = ruler' 0
  where ruler' n = interleave (streamRepeat n) (ruler' (n+1))

-- exercise 6: Fibonacci numbers in a stream

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

-- make Streams an instance of Num

instance Num (Stream Integer) where
  -- fromInteger is just the constant coefficient
  fromInteger i = Cons i (streamRepeat 0)
  -- to negate, just negate every coefficient
  negate (Cons i s) = Cons (negate i) (negate s)
  -- addition is pairwise addition of coefficients
  (Cons a as) + (Cons b bs) = Cons (a+b) (as + bs)
  -- multiplication is as described in the notes
  (Cons a0 a') * b@(Cons b0 b') = Cons (a0*b0) (streamMap (*a0) b' + a' * b)

-- make Streams an instance of Fractional

instance Fractional (Stream Integer) where
  (Cons a0 a') / (Cons b0 b') = q
    where q = Cons (a0 `div` b0) ((a' - q * b') / fromInteger b0)

-- now the amazing fibs3!

fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x * x))

-- exercise 7: Fibonacci numbers through matrix multiplication

data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
  show (Matrix a b c d) = "| " ++ show a ++ " " ++ show b ++ " |\n"
                          ++ "| " ++ show c ++ " " ++ show d ++ " |"

instance Num Matrix where
  fromInteger i = Matrix i 0 0 i
  negate (Matrix a b c d) = Matrix (negate a) (negate b) (negate c) (negate d)
  (Matrix a b c d) + (Matrix e f g h) =
    Matrix (a+e) (b+f) (c+g) (d+h)
  (Matrix a b c d) * (Matrix e f g h) =
    Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

-- the Fibonacci matrix
fibMatrix :: Matrix
fibMatrix = Matrix 1 1 1 0

-- get F(n) from a matrix
fibFromMatrix :: Matrix -> Integer
fibFromMatrix (Matrix _ f _ _) = f

-- fib4: log time Fibonacci numbers
-- the exercise says a special case is required for zero, but this is not the case?
fib4 :: Integer -> Integer
fib4 n = fibFromMatrix $ fibMatrix^n
