module Problem_022.Main where
import Data.Text (splitOn, Text, pack, unpack)
import Data.List (sort)
import Data.Char (ord)
{-
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
-}
-- Understanding haskell IO: https://stackoverflow.com/questions/11229854/how-can-i-parse-the-io-string-in-haskell
main :: IO Int
main = do
    content <- readFile "p022_names.txt"
    let xs = map (strToIntList . unpack) . sort . parseFile $ content
    let ts = zip [1..length xs] . map sum $ xs
    let reduced = map (uncurry (*)) ts
    let problem = sum reduced
    return problem

-- parses a file thats formatted like "p022_names.txt"
parseFile :: String -> [Text]
parseFile str = splitOn (pack ",") (pack signsRemoved)
          where signsRemoved = filter (\x -> x /= '\\' && x /= '"') str

-- Converts a String to a numerical representation
strToIntList :: String -> [Int]
strToIntList = map (\c -> ord c - 64)