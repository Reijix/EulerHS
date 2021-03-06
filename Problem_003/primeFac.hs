{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}
import Data.List
import qualified Data.Maybe

main = print (last (getPrimeFactors 600851475143))

-- Infinite list of primes calculated with the sieve erathostenes
primes :: [Int]
primes = 2 : sieve [3,5..]
  where sieve (p:ps) = p : sieve (filter (\x -> x `mod` p /= 0) ps)
        sieve _ = []

-- Returns the smallest primeFactor of a given number or 0 if the number is 1 or 0
primeFactor :: Int -> Int
primeFactor x = Data.Maybe.fromMaybe 0 nextprime
  where nextprime = find (\p -> x `mod` p == 0) (takeWhile (<= x) primes)


-- Gets all PrimeFactors of the given number
getPrimeFactors :: Int -> [Int]
getPrimeFactors n = map snd (takeWhile (\(x,y) -> y /= 0) (primeFactors n))
  where primeFactors n = tail (iterate nextPrimeFactor (n, 1))
        nextPrimeFactor (x,y) | primeFactor x == 0 = (0,0)
                              | otherwise = (x `div` primeFactor x, primeFactor x)
