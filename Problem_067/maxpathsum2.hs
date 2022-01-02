module Main where
{-
By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

    3
   7 4
  2 4 6
 8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:
-}

-- Understanding haskell IO: https://stackoverflow.com/questions/11229854/how-can-i-parse-the-io-string-in-haskell
main :: IO Int
main = do
    -- This time we parse the pyramid in main, because IO stuff...
    content <- readFile "p067_triangle.txt"
    let ls = lines content :: [String]
    let ws = map words ls :: [[String]]
    let pyramid = reverse (map (map read) ws) :: [[Int]]
    return (maxPath pyramid)


maxPath :: [[Int]] -> Int
maxPath [] = 0
maxPath [[x]] = x
maxPath (xs:xss) = maxPath (newXs:tail xss)
    where reducedXs = map (\x -> maximum [xs!!x, xs!!(x+1)]) [0..length xs - 2]
          newXs = zipWith (+) (head xss) reducedXs