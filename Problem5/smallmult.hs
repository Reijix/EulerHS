{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}
import Data.List

main = print (problem 20)

-- Checks if a is evenly divisible by b
isDivBy :: Int -> Int -> Bool
isDivBy a b = a `mod` b == 0

-- If a number is divisible by 9 it is also divisible by 3, so we dont have to check
-- This function shrinks our input accordingly
parseInput :: [Int] -> [Int]
parseInput ps = filter (\p -> (not (any (\x -> x /= p && isDivBy x p) ps))) ps


-- Finds the smallest positive number that is evenly divisible by all of the numbers from 1 to n
-- Solves our problem very slow
problem :: Int -> Int
problem n = let range = (parseInput [1..n]) in
    case (find (\x -> all (\y -> isDivBy x y) range)) [1..] of
    Just a -> a
    Nothing -> 0
