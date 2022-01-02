module Main where
{-
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

-- TODO

main = print (problem 1000000)

collatz :: Int -> Int
collatz n | n == 1 = 1
          | even n = collatz (n `div` 2)
          | otherwise = collatz (3*n + 1)

collatzStep :: Int -> Int
collatzStep n | even n = n `div` 2
              | otherwise = 3*n + 1

collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence n = n : collatzSequences !! (collatzStep n - 1)

collatzLengths :: [Int]
collatzLengths = 0 : 1 : map (\x -> 1 + collatzLengths !! collatzStep x) [2..]

collatzLength :: Int -> Int
collatzLength n | n == 1 = 1
                | otherwise = 1 + collatzLength (collatzStep n)

collatzLengthsArr = listArray (1,1000000000) (map f [1..1000000000])
                  where f n | n == 1 = 1
                            | otherwise = 1 + collatzLengthsArr!collatzStep n

collatzSequences :: [[Int]]
collatzSequences = map collatzSequence [1..]


-- TODO Spits out wrong result?? error in collatzLength???
-- >> function gets max length, we want the starting element...
problem :: Int -> (Int, Int)
problem n = foldl (\ (c,d) (a,b) -> if a > c then (a,b) else (c,d)) (1,1) colList
          where colList = zip (map collatzLength [1..n-1]) [1..n-1]

