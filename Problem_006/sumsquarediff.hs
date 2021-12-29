{-
Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-}

main = print (problem 100)

square :: Int -> Int
square x = x * x

sumTo :: Int -> Int
sumTo x = sum [1..x]

squareSumTo :: Int -> Int
squareSumTo x = sum (map square [1..x])

problem n = square (sumTo n) - squareSumTo n
