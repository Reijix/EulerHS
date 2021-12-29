{-
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
-}

{-
IDEAS:
- Last digit can be ignored when replacing, because numbers ending in 0,2,4,6,8 are not prime
-
-}

main = print 0

-- Transform int into a list of single digit integers
numToList :: Int -> [Int]
numToList x | x < 10 = [x]
            | otherwise = x `mod` 10 : (numToList (x `div` 10))

-- Extract third member of tuple
sel3 (_,_,x) = x

-- Reverse operation of numToList
listToNum :: [Int] -> Int
listToNum ps = sel3 (helper (ps, 1, 0))
    where helper (p:ps, n, res) = helper (ps, n * 10, res + n * p)
          helper ([], n, res) = ([], n, res)

-- Function that replaces all digits at indices in xs with number
replaceIdxWith :: [Int] -> [Int] -> Int -> [Int]
replaceIdxWith ps xs n = let idxList = zip ps [0..]
                         in map (\p -> case () of  
                                      _| elem (snd p) xs -> n 
                                       | otherwise -> fst p
                                ) idxList 

-- Gets possible different variations of a n long index list
-- example: getVariations[2] = [[0],[1],[0,1]]
getVariations :: Int -> [[Int]]
getVariations 0 = []
getVariations 1 = [[0]]
-- TODO


