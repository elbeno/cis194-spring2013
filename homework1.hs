import Data.Char

-- exercises 1-4: CC validation

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = map (fromIntegral . digitToInt) $ show n
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:ys) = x : y*2 : doubleEveryOther' ys

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- the exercise specifies that doubleEveryOther should work right-to-left, but
-- then what did we write toDigitsRev for? doubleEveryOther' works
-- left-to-right, and we don't need to reverse again because addition is
-- commutative...

validate :: Integer -> Bool
validate n = s `mod` 10 == 0
  where s = sumDigits . doubleEveryOther' $ toDigitsRev n

-- exercise 5: tower of hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b
                ++ [(a,b)]
                ++ hanoi (n-1) c b a
