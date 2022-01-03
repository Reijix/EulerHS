module Main where
{-
n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
-}
main :: IO ()
main = print . problem $ 100

numToList :: Integer -> [Integer]
numToList = map (\c -> read [c]) . show

fac :: Integer -> Integer
fac n = product [1..n]

problem :: Integer -> Integer
problem = sum . numToList . fac