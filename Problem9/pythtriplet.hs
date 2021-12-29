{-# LANGUAGE TupleSections #-}
module Main where
import Data.List
{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}
main = print (problem 1000)

problem :: Int -> Int
problem n = let
    triplets = filter (checkSum n) (findPyTriplets n)
    empty = null (findPyTriplets n)
    triplet = if empty then (0,0,0) else head triplets
    prod (a,b,c) = a*b*c
    in prod triplet

findPyTriplets :: Int -> [(Int, Int, Int)]
findPyTriplets n = filter isPythagorean (getTriplets n)

-- Checks if sum of a triplet equals n
checkSum :: Int -> (Int, Int, Int) -> Bool
checkSum n (a,b,c) = a+b+c == n

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a,b,c) = a*a + b*b == c*c

-- Gets all triplets that have a sum below n (performance optimization)
getTriplets :: Int -> [(Int, Int, Int)]
getTriplets n = concatMap (\ (a,b) -> map (a,b,) [b+1..(n-a-b)]) (concatMap (\a -> map (a,) [(a+1)..(n-a)]) [1..n-2])