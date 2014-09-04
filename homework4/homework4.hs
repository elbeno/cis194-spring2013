-- exercise 1: wholemeal programming

-- fun1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- fun1 filters out odd numbers
-- subtracts 2 from each even number
-- and then calculates the product

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

-- fun2

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- this is the Ulam/Collatz conjecture algorithm
-- we are summing the even numbers in the chain
-- and stopping at 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate collatz
  where collatz k | even k = k `div` 2
                  | otherwise = 3 * k + 1

-- exercise 2: folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- obtain the height of a tree

height :: Tree a -> Integer
height Leaf = -1
height (Node i _ _ _) = i

-- helper functions to insert on the left or right subtree

insertLeft :: a -> Tree a -> Tree a
insertLeft a (Node i l v r) = let l' = insert a l
                              in Node (max (height l') (height r) + 1) l' v r

insertRight :: a -> Tree a -> Tree a
insertRight a (Node i l v r) = let r' = insert a r
                              in Node (max (height l) (height r') + 1) l v r'

-- insertion into a Leaf is the base case
-- insert on the left if that's smaller
-- insert on the right if that's smaller
-- otherwise try inserting on the right; if it makes a height change, insert on the left
-- this gives a full balanced tree

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a t@(Node i l v r)
  | height l < height r = insertLeft a t
  | height l > height r = insertRight a t
  | otherwise = let t' = insertRight a t
                in if height t' == i
                   then t'
                   else insertLeft a t

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- exercise 3: more folds


-- part 1: xor
xor :: [Bool] -> Bool
xor = foldl (\x y -> x && not y || y && not x) False

-- part 2: map implemented with fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- part 3: foldl implemented with foldr

-- We need to build up a function that when applied to base will yield the right
-- result.
myFoldlFn :: (a -> b -> a) -> a -> [b] -> (a -> a)
-- The base case is just the id function
myFoldlFn f base [] = id
-- The recursive case is the function that will apply f to the result of folding
-- the rest of the list applied to its argument, and the list element
myFoldlFn f base (x:xs) = \v -> f ((myFoldlFn f base xs) v) x

-- this function can be built up with foldr
myFoldlFn' f xs = foldr (\x g -> (\v -> g (f v x))) id xs
-- or equivalently
myFoldlFn'' f xs = foldr (\x g v -> g (f v x)) id xs

-- now we need to apply that function over the list to get an all-encompassing
-- function which we then apply to the base
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = (foldr (\x g v -> g (f v x)) id xs) base

-- exercise 4: finding primes with the Sieve of Sundaram

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : map ((+1) . (*2)) ps
  where cs = [c | j <- [1..n], i <- [1..j], c <- [i+j+2*i*j], c <= n]
        ps = filter (not . flip elem cs) [1..n]
