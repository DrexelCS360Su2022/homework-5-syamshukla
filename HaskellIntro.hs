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
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

toDigits :: Integer -> [Integer]

toDigits x
  | x <= 0 = []
  | otherwise = toDigits (dropLastDigit x) ++ [lastDigit x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) 
  | even (length (x:y:xs)) = x*2 : y : doubleEveryOther xs
  | otherwise = x : y*2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:y) = sum (toDigits x) + sumDigits y

validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow f 0 x = x
pow f n x = f (pow f (n-1) x)

g :: Integer -> Integer
g (x) | x == 0 = 0
      | otherwise = x - (pow g  1 (g (fromIntegral x-1)))
 
h :: Integer -> Integer
h (x) | x == 0 = 0
      | otherwise = x - (pow h  1 (h (h (fromIntegral x -1))))

d :: Int -> Integer -> Integer
d (x) | x == 0 = 0
      | x == 2 = g (fromIntegral x)
      | x == 3 = h (fromIntegral x)
      | otherwise = x - (pow d  1 (d (d (fromIntegral x -1))))

--
-- Problem 3
--

powerSet = error "powerSet not yet defined"
