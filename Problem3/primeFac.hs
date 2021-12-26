{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}
import Data.List

main = print 0


primes :: [Int]
primes = 2 : 3 : sieve [5,7..]
  where sieve (p:ps) = p : sieve (filter (\x -> (x `mod` p /= 0)) ps)

primeFactor :: Int -> Int
primeFactor x = case nextprime of
    Just a -> a
    Nothing -> 0
  where nextprime = find (\p -> x `mod` p == 0) (takeWhile (< x) primes)

getPrimeFactors :: Int -> [Int]
getPrimeFactors n = map (\(x,y) -> y) (takeWhile (\(x,y) -> x /= 0) (primeFactors n)) 
  where primeFactors n = tail (iterate nextPrimeFactor (n, 1))
        nextPrimeFactor (x,y) | primeFactor x == 0 = (0,0)
                              | primeFactor x /= 0 = (x `div` (primeFactor x), primeFactor x)

