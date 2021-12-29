module Main where
{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

main = print (primeSum 2000000)

isPrime n = go 2
  where
    go d
      | d*d > n        = True
      | n `rem` d == 0 = False
      | otherwise      = go (d+1)

primes = 2 : filter isPrime [3,5.. ]

-- Sums up all primes below n
primeSum :: Int -> Int
primeSum n = sum . takeWhile (< n) $ primes
