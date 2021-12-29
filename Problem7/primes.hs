{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

-- Our list is 0-indexed
main = print (primes !! 10002)

-- Reusing our primes list from Problem 3
primes :: [Int]
primes = 2 : sieve [3,5..]
  where sieve (p:ps) = p : sieve (filter (\x -> x `mod` p /= 0) ps)
        sieve _ = []
