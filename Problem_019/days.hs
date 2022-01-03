module Main where
{-
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-}

-- Bonus challenge: dont use haskell date libraries!!

main = print problem

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)


-- Starting from 1 Jan 1900 we list each weekday
weekDays :: [Day]
weekDays =  cycle [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

-- List of all dates starting at 01-01-1900 (Format DD-MM-YYYY)
dates :: [(Int, Int, Int)]
dates = iterate getNextDate (1,1,1900)
    where getNextDate (d,m,y) | d < 28 = (d+1,m,y)
                              | m `elem` [4,6,9,11] && d == 30 = (1,m+1,y)
                              | m `elem` [1,3,5,7,8,10] && d == 31 = (1,m+1,y)
                              | m == 2 && d == 29 && (y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0) = (1, 3, y)
                              | m == 2 && d == 28 && not (y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0) = (1, 3, y)
                              | m == 12 && d == 31 = (1,1,y+1)
                              | otherwise = (d+1,m,y)

problem = foldl (\a (w,(d,m,y)) -> if d == 1 && w == Sunday then a+1 else a) 0 xs
        where ds = zip weekDays dates
              xs = takeWhile (\(_, (d,m,y)) -> not (d == 1 && m == 1 && y == 2001)) (dropWhile (\(_, (d,m,y)) -> not (d==1 && m==1 && y==1901)) ds)