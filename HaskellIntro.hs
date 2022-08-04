{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = (x - lastDigit x) `div` 10

toDigits :: Integer -> [Integer]
toDigits x = 
    if x <= 0 
    then []
    else toDigits (dropLastDigit x) ++ [lastDigit x]

doubleEveryOtherReverse :: [Integer] -> Integer -> [Integer]
doubleEveryOtherReverse [] shouldDouble = []
doubleEveryOtherReverse x shouldDouble =
    if shouldDouble == 1
    then head x * 2 : doubleEveryOtherReverse (tail x) 0
    else head x : doubleEveryOtherReverse (tail x) 1


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherReverse (reverse x) 0)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = 
    if head x >= 10
    then sumDigits (toDigits (head x)) + sumDigits (tail x)
    else head x + sumDigits (tail x)

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

--
-- Problem 2
--

square :: Int -> Int
square x = x * x

pow :: (a -> a) -> Int -> a -> a
pow f 0 = id
pow f n =
    if n == 1
    then f
    else f . pow f (n - 1)

g :: Integer -> Integer
g 0 = 0
g x = x - pow g 2 (x - 1)

h :: Integer -> Integer
h 0 = 0
h x = x - pow h 3 (x - 1)

d :: Int -> Integer -> Integer
d i 0 = 0
d i x = x - pow (d i) i (x - 1)


--
-- Problem 3
--

powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet xs =
    if isEmpty xs
    then singleton empty
    else mapSet (insert (fst (split xs))) (powerSet (snd (split xs)))
         `union` 
         powerSet (snd (split xs))