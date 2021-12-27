{-
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

main = print (maximum palProds)

-- Checks if a given List is a palindrome
isPalindrome :: [Int] -> Bool
isPalindrome (x:[]) = True
isPalindrome [] = True
isPalindrome ps = (head ps == last ps) && (isPalindrome ((init . tail) ps))

-- Converts a number to a list
numToList :: Int -> [Int]
numToList x | x < 10 = [x]
            | x >= 10 = x `mod` 10 : (numToList (x `div` 10))

threeDig = [100..999]

-- Gets the cross product of threeDig with itself, multiplies the tuples and filters out any non
-- palindromic numbers
palProds = filter (\x -> isPalindrome (numToList x)) (concat (map (\x -> (map (\p -> x * p) threeDig)) threeDig))
