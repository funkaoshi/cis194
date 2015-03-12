{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = (n - lastDigit n) `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0    = []
  | otherwise = [lastDigit n] ++ toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:y:zs) = x : (2*y) : doubleEveryOther zs

-- Exercise 4 -----------------------------------------


-- calculate sum of the digits in a two digit positive number
-- should make the parameters a type (numbers 0-99)
sumTwoDigits :: Integer -> Integer
sumTwoDigits x = lastDigit x + dropLastDigit x

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sumTwoDigits x + sumDigits xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
-- Note: credit card numbers are 16 digits long.
luhn :: Integer -> Bool
luhn x = lastDigit (sumDigits (toRevDigits x)) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

-- Returns the list of moves required to move "n" pegs from the first peg to
-- the second peg, using the 3rd peg as temporary storage during the process.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 x y z = []
hanoi n x y z = hanoi (n-1) x z y ++ [(x, y)] ++ hanoi (n-1) y z x

-- Exercise 7 -----------------------------------------

-- Towers of Hanoi with 4 pegs
-- As above, returns a list of moves required to move "n" pegs from the first
-- peg to the second peg.
-- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]

