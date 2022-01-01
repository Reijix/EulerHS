module Main where
{-
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
-}
main = print (getPathNum 0 0)

data Direction = DRight | DDown deriving (Show, Eq)

height :: Int
height = 2
width :: Int
width = 2

-- Calculate pathlength from (0,0) to (width, height)
taxiLen :: Int
taxiLen = iSqrt (height^2 * width^2)

iSqrt :: Int -> Int
iSqrt = floor . sqrt . fromIntegral

getPossibleDirs :: Int -> Int -> [Direction]
getPossibleDirs x y = [DRight | x < width] ++ [DDown | y < height]

getPaths :: Int -> Int -> [Direction] -> [[Direction]]
getPaths x y curr | x == width && y == height = [curr]
                  | x > width || y > height = []
                  | otherwise = getPaths (x+1) y (DRight:curr) ++ getPaths x (y+1) (DDown:curr)

getPathNum :: Int -> Int -> Int
getPathNum x y | x == width && y == height = 1
               | x > width || y > height = 0
               | otherwise = getPathNum (x+1) y + getPathNum x (y+1)

latticePaths :: Integer -> Integer -> Integer
latticePaths w h = let x = w + h; y = w in
    fac x `div` (fac y * fac (x - y))

fac :: Integer -> Integer
fac 0 = 1
fac n = product [1..n]

