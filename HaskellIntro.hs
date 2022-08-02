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

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = error "doubleEveryOther not yet defined"

sumDigits :: [Integer] -> Integer
sumDigits = error "sumDigits not yet defined"

validate :: Integer -> Bool
validate = error "validate not yet defined"

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow = error "pow not yet defined"

g :: Integer -> Integer
g = error "g not yet defined"

h :: Integer -> Integer
h = error "h not yet defined"

d :: Int -> Integer -> Integer
d = error "d not yet defined"

--
-- Problem 3
--

powerSet = error "powerSet not yet defined"
