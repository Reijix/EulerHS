module Main where
import Data.Array
{-
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
-}


-- This takes a while :D
main :: IO ()
main = print . problem $ 10000

getDivisors :: Int -> [Int]
getDivisors n = filter (\x -> n `mod` x == 0) [1..(n `div` 2)]

divSum :: Int -> Int
divSum = sum . getDivisors

divSums :: Int -> Array Int Int
divSums n = listArray (1,n) . map divSum $ [1..n]

amicableNumbers :: Int -> [(Int, Int)]
amicableNumbers n = [(x,y) | x <- [2..n], y <- [2..n], x /= y && ls ! x == y && ls ! y == x]
                  where ls = divSums n

problem :: Int -> Int
problem = sum . map fst . amicableNumbers