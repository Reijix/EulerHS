module Main where
{-
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
The use of "and" when writing out numbers is in compliance with British usage.
-}

dict :: [String]
dict = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty"]

translateOnes :: Int -> String
translateOnes n | num == 0 = ""
                | otherwise = dict !! num
                where num = n `mod` 10

translateTens :: Int -> String
translateTens n | num == 0 = ""
                | num == 10 = "ten"
                | num == 20 = "twenty"
                | num == 30 = "thirty"
                | num == 40 = "forty"
                | num == 50 = "fifty"
                | num == 80 = "eighty"
                | otherwise = dict !! (num `div` 10) ++ "ty"
                where num = ((n `mod` 100) `div` 10) * 10

translateHundreds :: Int -> String
translateHundreds n | num2 == 0 = dict !! num ++ "hundred"
                    | num == 0 = ""
                    | otherwise = dict !! num ++ "hundredand"
                where num = (n `mod` 1000) `div` 100
                      num2 = n `mod` 100

-- translates words below 1000
translateWord :: Int -> String
translateWord n | n <= 19 = dict !! n
                | n < 100 = translateTens n ++ translateOnes n
                | n < 1000 && num < 20 && num > 0 = translateHundreds n ++ dict !! num
                | n < 1000 = translateHundreds n ++ translateTens n ++ translateOnes n
                | otherwise = ""
                where num = n `mod` 100


dictTens :: [String]
dictTens = ["", "ten", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

natWords :: [String]
natWords = tail dict ++ map translateWord [21..999] ++ ["onethousand"]

lengths :: [Int]
lengths = map length natWords

main = print (sum lengths)