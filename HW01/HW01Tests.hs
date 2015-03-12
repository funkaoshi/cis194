-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, ns) = toRevDigits n == ns

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(0, []), (-1, []), (-12, []), (1, [1]), (12, [2,1]), (123, [3,2,1])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (xs, ys) = doubleEveryOther xs == ys

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([], []), ([0], [0]), ([0,0], [0,0]), ([2,2],[2,4]), ([-1,-1], [-1, -2])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigit :: ([Integer], Integer) -> Bool
testSumDigit (xs, expected) = sumDigits xs == expected

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigit
             [([], 0), ([1], 1), ([1, 11], 3), ([10, 5, 18, 4], 19)]
           ]

-- Exercise 5 -----------------------------------------

testLuhnNumber :: (Integer, Bool) -> Bool
testLuhnNumber (x, expected) = luhn x == expected

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhnNumber
             [(5594589464218858, True)]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
