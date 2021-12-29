{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
-}
nat = [1..]

isMul35 :: Int -> Bool
isMul35 x = (x `mod` 3) == 0 || (x `mod` 5) == 0

sumMul35 :: Int -> Int
sumMul35 x = sum (filter isMul35 (take x nat)) 


main = print (sumMul35 999)
