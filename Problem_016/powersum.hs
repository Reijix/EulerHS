module Main where
{-
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
-}

main = print (problem 1000)

problem :: Integer -> Integer 
problem n = sum (numToList (2^n))

-- Converts a number to a list
numToList :: Integer -> [Integer]
numToList x | x < 10 = [x]
            | otherwise = x `mod` 10 : numToList (x `div` 10)