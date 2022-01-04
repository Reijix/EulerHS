module Main where
import Data.List ( (\\), delete, groupBy, sort, sortBy )
import Data.Function (on)
{-
In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

The file, poker.txt, contains one-thousand random hands dealt to two players. 
Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. 
You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win?
-}

{--------------------------------------------BEGIN - TYPES-------------------------------------------------------------}

type Card = (Rank, Suit)
type Hand = [Card]
type Game = (Hand, Hand)

data Suit = Clubs | Hearts | Spades | Diamonds deriving (Show, Eq)
data Strength = HighCard | Pair | TwoPair | ThreeKind | Straight | Flush | FullHouse | FourKind | StraightFlush | RoyalFlush deriving (Show, Eq, Ord)
data Rank = NULL | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

nextRank :: Rank -> Rank
nextRank NULL = NULL
nextRank Two = Three
nextRank Three = Four
nextRank Four = Five
nextRank Five = Six
nextRank Six = Seven
nextRank Seven = Eight
nextRank Eight = Nine
nextRank Nine = Ten
nextRank Ten = Jack
nextRank Jack = Queen
nextRank Queen = King
nextRank King = Ace
nextRank Ace = Two

{----------------------------------------------END - TYPES-------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------------------------}
{--------------------------------------------BEGIN - TUPLES------------------------------------------------------------}
fst3 :: (a0, b0, c0) -> a0
fst3 (a,_,_) = a

snd3 :: (a0, b0, c0) -> b0
snd3 (_,b,_) = b

trd3 :: (a0, b0, c0) -> c0
trd3 (_,_,c) = c
{---------------------------------------------END - TUPLES-------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------------------------}
{--------------------------------------------BEGIN - PARSING-----------------------------------------------------------}

parseRank :: Char -> Rank
parseRank c | c == '2' = Two
            | c == '3' = Three
            | c == '4' = Four
            | c == '5' = Five
            | c == '6' = Six
            | c == '7' = Seven
            | c == '8' = Eight
            | c == '9' = Nine
            | c == 'T' = Ten
            | c == 'J' = Jack
            | c == 'Q' = Queen
            | c == 'K' = King
            | c == 'A' = Ace
            | otherwise = undefined

parseSuit :: Char -> Suit
parseSuit c | c == 'C' = Clubs
            | c == 'H' = Hearts
            | c == 'S' = Spades
            | c == 'D' = Diamonds
            | otherwise = undefined -- Not good, but the exercise says we can expect all hands to be valid

-- Gets a 2 character string and returns a type card
parseCard :: String -> Card
parseCard [c,d] = (parseRank c, parseSuit d)
parseCard o = undefined

parseGames :: String -> [Game]
parseGames = map (splitAt 5 . map parseCard . words) . lines

{--------------------------------------------END - PARSING-------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------------------------}
{-------------------------------------------BEGIN - EVALUATING---------------------------------------------------------}

-- evaluates a game and returns the winner (1 or 2)
-- NOTE: in each game theres a clear winner! No need to check for draws!
evalGame :: Game -> Int
evalGame (h1, h2)
  | fst3 eval1 > fst3 eval2 = 1
  | fst3 eval2 > fst3 eval1 = 2
  | winnerSame /= 0 = winnerSame
  | otherwise = evalHighCards (hc1, hc2)
  where
      winnerSame = evalSameRank (fst3 eval1 , snd3 eval1, snd3 eval2)
      sorted1 = sortBy (compare `on` fst) h1
      sorted2 = sortBy (compare `on` fst) h2
      eval1 = evalHand sorted1
      eval2 = evalHand sorted2
      hc1 = trd3 eval1
      hc2 = trd3 eval2

evalSameRank :: (Strength, [Card], [Card]) -> Int
evalSameRank (r, h1, h2) | r == HighCard = compareHighcards (h1,h2)
                         | r == Pair = comparePairs (h1,h2)
                         | r == TwoPair = compareTwoPairs(h1,h2)
                         | r == ThreeKind = compareThreeKinds (h1,h2)
                         | r == Straight = compareStraights (h1,h2)
                         | r == Flush = compareFlushes (h1,h2)
                         | r == FullHouse = compareFullHouses (h1,h2)
                         | r == FourKind = compareFourKinds (h1,h2)
                         | r == StraightFlush = compareStraightFlushes (h1,h2)
                         | otherwise = 0

compareHighcards :: (Hand, Hand) -> Int
compareHighcards (h1, h2)
  | c1 > c2 = 1
  | c2 > c1 = 2
  | otherwise = 0
  where
      c1 = fst . head $ h1
      c2 = fst . head $ h2

comparePairs :: (Hand, Hand) -> Int
comparePairs (h1, h2)
  | c1 > c2 = 1
  | c2 > c1 = 2
  | otherwise = 0
  where
      c1 = fst . head $ h1
      c2 = fst . head $ h2

-- NOTE: groupby isnt really needed, evaltwopairs returns the nums ordered
compareTwoPairs :: (Hand, Hand) -> Int
compareTwoPairs (h1, h2)
  | head pairs1 > head pairs2 = 1
  | head pairs2 > head pairs1 = 2
  | (head . tail $ pairs1) > (head . tail $ pairs2) = 1
  | (head . tail $ pairs2) > (head . tail $ pairs1) = 2
  | otherwise = 0
  where
      group1
        = filter (\ x -> length x > 1)
            . groupBy (\ (r, s) (a, b) -> r == a)
            $ h1
      group2
        = filter (\ x -> length x > 1)
            . groupBy (\ (r, s) (a, b) -> r == a)
            $ h2
      pairs1
        = reverse . sort
            $ [fst . head . head $ group1, fst . head . head . tail $ group1]
      pairs2
        = reverse . sort
            $ [fst . head . head $ group2, fst . head . head . tail $ group2]

compareThreeKinds :: (Hand, Hand) -> Int
compareThreeKinds (h1, h2)
  | c1 > c2 = 1
  | c2 > c1 = 2
  | otherwise = 0
  where
      c1 = fst . head $ h1
      c2 = fst . head $ h2

compareStraights :: (Hand, Hand) -> Int
compareStraights (h1, h2)
  | c1 > c2 = 1
  | c2 > c1 = 2
  | otherwise = 0
  where
      c1 = fst . head $ h1
      c2 = fst . head $ h2

compareFlushes :: (Hand, Hand) -> Int
compareFlushes (h1, h2)
  | c1 > c2 = 1
  | c2 > c1 = 2
  | otherwise = 0
  where
      sorted1 = sortBy (compare `on` fst) h1
      sorted2 = sortBy (compare `on` fst) h2
      c1 = fst . last $ sorted1
      c2 = fst . last $ sorted2

compareFullHouses :: (Hand, Hand) -> Int
compareFullHouses (h1, h2)
  | head11 > head21 = 1
  | head21 > head11 = 2
  | head12 > head22 = 1
  | head22 > head12 = 2
  | otherwise = 0
  where
      group1
        = filter (\ x -> length x > 1)
            . groupBy (\ (r, s) (a, b) -> r == a)
            $ h1
      group2
        = filter (\ x -> length x > 1)
            . groupBy (\ (r, s) (a, b) -> r == a)
            $ h2
      pairs1
        = sortBy (flip (\ a b -> compare (length a) (length b))) group1
      pairs2
        = sortBy (flip (\ a b -> compare (length a) (length b))) group2
      head11 = fst . head . head $ pairs1
      head12 = fst . head . head . tail $ pairs1
      head21 = fst . head . head $ pairs2
      head22 = fst . head . head . tail $ pairs2

compareFourKinds :: (Hand, Hand) -> Int
compareFourKinds (h1, h2)
  | c1 > c2 = 1
  | c2 > c1 = 2
  | otherwise = 0
  where
      c1 = fst . head $ h1
      c2 = fst . head $ h2

compareStraightFlushes :: (Hand, Hand) -> Int
compareStraightFlushes = compareStraights

evalHighCards :: ([Card], [Card]) -> Int
evalHighCards (h1,h2) = foldl (\a ((r1,s1),(r2,s2)) -> if a /= 0 then a else if r1 > r2 then 1 else if r2 > r1 then 2 else 0) 0 . uncurry zip $ (sorted1,sorted2)
    where sorted1 = sortBy (flip compare `on` fst) h1
          sorted2 = sortBy (flip compare `on` fst) h2

-- values a hand and then returns a triple: (Rank, Cards in Rank, Highcards)
evalHand :: Hand -> (Strength, [Card], [Card])
evalHand h | fst royalflush /= [] = (RoyalFlush, fst royalflush, snd royalflush)
           | fst straightflush /= [] = (StraightFlush, fst straightflush, snd straightflush)
           | fst fourkind /= [] = (FourKind, fst fourkind, snd fourkind)
           | fst fullhouse /= [] = (FullHouse, fst fullhouse, snd fullhouse)
           | fst flush /= [] = (Flush, fst flush, snd flush)
           | fst straight /= [] = (Straight, fst straight, snd straight)
           | fst threekind /= [] = (ThreeKind, fst threekind, snd threekind)
           | fst twopair /= [] = (TwoPair, fst twopair, snd twopair)
           | fst pair /= [] = (Pair, fst pair, snd pair)
           | otherwise = (HighCard , fst highcard, snd highcard)
        where highcard = evalHighCard h
              pair = evalPair h
              twopair = evalTwoPair h
              threekind = evalThreeKind h
              straight = evalStraight h
              flush = evalFlush h
              fullhouse = evalFullHouse h
              fourkind = evalFourKind h
              straightflush = evalStraightFlush h
              royalflush = evalRoyalFlush h



-- Functions for evaluating strengths:

evalHighCard :: Hand -> ([Card], [Card])
evalHighCard h = ([max],delete max h)
    where max = foldl (\(a,b) (r,s) -> if r > a then (r,s) else (a,b)) (NULL,Spades) h

-- NOTE: when evalPair is called we know that there are no ThreeKind or FourKind
-- NOTE: also: theres only a single pair, because evalTwoPair has already been called!!
evalPair :: Hand -> ([Card], [Card])
evalPair h = if pairFound then (head groups, h \\ head groups) else ([], h)
        where groups = filter (\x -> length x > 1) . groupBy (\(r,s) (a,b) -> r == a) $ h
              pairFound = groups /= []
              -- maxGroup = foldl (\ ((a,b):xs) ((r,s):ys) -> if a >= r then (a,b):xs else (r,s):ys) [(NULL, Spades)] groups

evalTwoPair :: Hand -> ([Card], [Card])
evalTwoPair h = if length groups == 2 then (hand, h \\ hand) else ([], h)
        where groups = filter (\x -> length x > 1) . groupBy (\(r,s) (a,b) -> r == a) $ h
              hand = concat groups

-- NOTE: when evalThreeKind is called there cant be a FullHouse in the Hand
-- NOTE: when theres a ThreeKind, there cant be another group (see above), so no need to search for the maximum pair
evalThreeKind :: Hand -> ([Card], [Card])
evalThreeKind h = if pairFound && (length . head $ groups) == 3 then (head groups, h \\ head groups) else ([], h)
        where groups = filter (\x -> length x > 1) . groupBy (\(r,s) (a,b) -> r == a) $ h
              pairFound = groups /= []

evalStraight :: Hand -> ([Card], [Card])
evalStraight h = if (notElem King mapped || notElem Two mapped) && isStraight then (h, []) else ([], h)
        where sorted = sortBy (compare `on` fst) h
              zipped = zip (take 4 sorted) . tail $ sorted
              isStraight = foldl (\a ((b,c),(r,s)) -> nextRank b == r && a) True zipped
              mapped = map fst h

evalFlush :: Hand -> ([Card], [Card])
evalFlush h = if groupFound && (length . head $ groups) == 5 then (h, []) else ([], h)
        where groups = groupBy (\(r,s) (a,b) -> s == b) h
              groupFound = groups /= []

evalFullHouse :: Hand -> ([Card], [Card])
evalFullHouse h = if length groups == 2 && grouplen1 + grouplen2 == 5 then (concat groups, []) else ([], h)
        where groups = filter (\x -> length x > 1) . groupBy (\(r,s) (a,b) -> r == a) $ h
              grouplen1 = length . head $ groups
              grouplen2 = length . head . tail $ groups

evalFourKind :: Hand -> ([Card], [Card])
evalFourKind h = if pairFound && (length . head $ groups) == 4 then (head groups, h \\ head groups) else ([], h)
        where groups = filter (\x -> length x > 1) . groupBy (\(r,s) (a,b) -> r == a) $ h
              pairFound = groups /= []

evalStraightFlush :: Hand -> ([Card], [Card])
evalStraightFlush h = if (fst . evalStraight $ h) /= [] && (fst . evalFlush$ h) /= [] then (h, []) else ([], h)

evalRoyalFlush :: Hand -> ([Card], [Card])
evalRoyalFlush h = if (fst . evalStraightFlush $ h) /= [] && Ace `elem` mapped && Ten `elem` mapped then (h, []) else ([], h)
        where mapped = map fst h

{---------------------------------------------END - EVALUATING---------------------------------------------------------}
{----------------------------------------------------------------------------------------------------------------------}
{-------------------------------------------BEGIN - MAIN---------------------------------------------------------------}

main :: IO ()
main = do
    content <- readFile "p054_poker.txt"
    let games = parseGames content
    -- For debugging
    mapM_ printGame games

    let result = foldl (\a g -> if evalGame g == 1 then a + 1 else a) 0 games
    print result

printGame :: Game -> IO ()
printGame (h1, h2) = do
    print "----------------"
    print (h1, h2)
    let sorted1 = sortBy (compare `on` fst) h1
    let sorted2 = sortBy (compare `on` fst) h2
    print . evalHand $ sorted1
    print . evalHand $ sorted2
    print . evalGame $ (h1, h2)
    print "----------------"

testHand :: IO ()
testHand = do
    let game1 = ([(),(),(),(),()],[(),(),(),(),()])
    let pair = [(Two,Spades),(Two,Hearts),(Three, Spades),(Four, Spades),(Five, Spades)]
    let straight = [(Two,Spades), (Three,Spades), (Four,Spades), (Five,Spades), (Six,Spades)]
    print "Eval pair:"
    print . evalHand $ pair
    print "Eval straight:"
    print . evalHand $ straight


test :: IO ()
test = do
    let pair = [(Two,Spades),(Two,Hearts),(Three, Spades),(Four, Spades),(Five, Spades)]
    let straight = [(Two,Spades), (Three,Spades), (Four,Spades), (Five,Spades), (Six,Spades)]
    print "evalStraight straight:"
    print . evalStraight $ straight
    print "evalStraight pair:"
    print . evalStraight $ pair
    print "evalPair pair:"
    print . evalPair $ pair
    print "evalFlush pair: (is not Flush)"
    print . evalFlush $ pair
    print "evalFlush straight: (is Flush)"
    print . evalFlush $ straight