--  File     : Proj1.hs
--  Author   : Jizhe Hou
--  Purpose  : project1 Card Guess
--  Funtion  : 1. feedback     : based on the answer card and guess card, return 5 int value for the guesser
--                               1. how many of the cards in the answer are also in the guess
--                               2. how many cards in the answer have the rank lower than the lowest rank in the guess
--                               3. how many of the cards in the answer have the same rank as a card in the guess
--                               4. how many cards in the answer have the rank higher than the highest rank in the guess
--                               5. how many of the cards in the answer have than same suit as a card in the guess
--             2. initialGuess : get the input of how manys cards, inital the first guess and all the gameState of whole possible
--             3. nextGuess    : based on the last guess and the feedback to decide which is the best next guess
--  Date     :  12/09/2016

module Proj1 (feedback, initialGuess, nextGuess, GameState) where  

import Card
import Data.List
import Data.Function (on)

type GameState = [[Card]]

-- main funciton of the feedback
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [][] = (0,0,0,0,0)
feedback answer guess
    =  (eqNum answer guess,
        lowNum answer guess,
        sameRank answer guess,
        highNum answer guess,
        sameSuit answer guess)
        
-- calculate how many of the cards in the answer are also in the guess
eqNum :: [Card] -> [Card] -> Int   
eqNum [] _ = 0
eqNum (card:answer) guess = 
    compareCard card guess + eqNum answer guess
    
-- compare the each Card in the guess and answer, if equal the number plus one
compareCard :: Card -> [Card] -> Int
compareCard _ [] = 0
compareCard card (cardGuess:guess)
    | card == cardGuess = 1
    | otherwise = compareCard card guess

-- calculate how many of the cards in the answer have the rank lower than the lowest rank in the guess
lowNum :: [Card] -> [Card] -> Int 
lowNum [] _ = 0
lowNum ((Card s1 r1):answer) guess =
    let Card s2 r2 = lowestCard guess in 
    if r1 < r2 then 1 + lowNum answer guess 
    else lowNum answer guess

-- compare two card to decide which has lower rank than another
lowCard :: Card -> Card -> Card
lowCard (Card s1 r1) (Card s2 r2) =
    let rankOrder = compare r1 r2 in
    if rankOrder == LT then Card s1 r1 else Card s2 r2

-- find the lowest rank in the guess
lowestCard :: [Card] -> Card 
lowestCard (card1:card2:guess) =
    let minCard = lowCard card1 card2 in
    if length guess == 0 then minCard
    else 
        if minCard == card1 then lowestCard (card1:guess) 
        else lowestCard (card2:guess)

-- calculate how many of the cards in the answer have the rank higher than the highest rank in the guess
highNum :: [Card] -> [Card] -> Int 
highNum [] _ = 0
highNum ((Card s1 r1):answer) guess =
    let Card s2 r2 = highestCard guess in 
    if r1 > r2 then 1 + highNum answer guess 
    else highNum answer guess

-- compare two cards's rank to decide which is higher
highCard :: Card -> Card -> Card
highCard (Card s1 r1) (Card s2 r2) =
    let rankOrder = compare r1 r2 in
    if rankOrder == GT then Card s1 r1 else Card s2 r2

-- find the highest rank in the guess
highestCard :: [Card] -> Card 
highestCard (card1:card2:guess) =
    let maxCard = highCard card1 card2 in
    if length guess == 0 then maxCard
    else 
        if maxCard == card1 then highestCard (card1:guess) 
        else highestCard (card2:guess)

-- calculate how many cards have the same rank with cards in the guess        
sameRank :: [Card] -> [Card] -> Int 
sameRank [] _ = 0
sameRank (card:answer) guess = 
    compareRank card guess + sameRank answer guess

-- compare two cards' rank, if equal the number plus one
compareRank :: Card -> [Card] -> Int
compareRank _ [] = 0
compareRank (Card s1 r1) ((Card s2 r2) : guess)
    | r1 == r2 = 1
    | otherwise = compareRank (Card s1 r1) guess

-- calculate how many cards have the same suit with cards in the guess     
sameSuit :: [Card] -> [Card] -> Int 
sameSuit [] _ = 0
sameSuit (card:answer) guess = 
    compareSuit card guess + sameSuit answer guess

-- compare two cards' suit, if equal the number plus one
compareSuit :: Card -> [Card] -> Int
compareSuit _ [] = 0
compareSuit (Card s1 r1) ((Card s2 r2) : guess)
    | s1 == s2 = 1
    | otherwise = compareSuit (Card s1 r1) guess

    
-- first guess for the game and bulit the initial GameState for the guesser
initialGuess :: Int -> ([Card],GameState)
initialGuess num 
    | num == 2 = ([Card Heart R6, Card Spade Jack], gameState )
    | num == 3 = ([Card Heart R5, Card Spade R9, Card Club Queen], gameState)
    | num == 4 = ([Card Heart R4, Card Spade R7, Card Club R10, Card Diamond King], gameState)
    where 
    gameState = initialGameState num
                          
-- bulid the initial GameState (all the possible of the guess)
initialGameState :: Int -> GameState
initialGameState n = filter dupFilter gameState
    where
    gameState = sequence (allGameState n)
 
-- insert all cards into the GameState
allGameState :: Int -> GameState
allGameState 1 = [allCards]
allGameState num = allCards : allGameState(num-1)

allCards = [minBound..maxBound]

-- remove the guess which have same cards in a guess or the elements in the guess are same whic are only in different order
dupFilter :: Eq a => [a] -> Bool
dupFilter []        = True
dupFilter (x:xs)
    | xs == []      = True
    | x == head xs  = False
    | otherwise     = not (x `elem` xs) && dupFilter xs
 
 

-- use the last guess and the feedback to decide which is the next guess
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess,gameState) (eqNum, lowNum, sameRank, highNum, sameSuit) 
    -- if the correct cards are equal to the num of cards in a guess, so this guess is the correct one
    | length gameState == 0 = (guess,[guess])
    | eqNum == length guess = (guess,[guess])
    | otherwise             = (newGuess, newGameState)
    where
        newGuess 
        -- if the result of the GameState are too huge, just choose the first one in the GameState
        -- otherwise use deterNextGuess function to calculate which is the best next guess
            | length guess == 3 && length newGameState > 1000  = head newGameState
            | length guess == 4 && length newGameState > 1000  = head newGameState
            | length guess == 3 && length newGameState <= 1000  = deterNextGuess newGameState
            | length guess == 4 && length newGameState <= 1000  = deterNextGuess newGameState
            | otherwise                                        = deterNextGuess newGameState 
        newGameState = filterGameState guess (eqNum, lowNum, sameRank, highNum, sameSuit) gameState

-- use the feedback to filter the incorrect guess
-- only the possible guess which match the last guess get the same feedback can be the correct guess
filterGameState :: [Card] -> (Int,Int,Int,Int,Int) -> GameState -> GameState
filterGameState guess fk gamestate = filter (\card -> (feedback card guess) == fk) gamestate


-- find the best guess which to make the mathematical expectation of the probalility smallest
deterNextGuess :: GameState -> [Card]
deterNextGuess [] = []
deterNextGuess gameState = fst $ head $ sortBy (compare `on` snd) $ calAllLeftProb $ calAllGuessFebk gameState gameState

-- Probability of cards left for the guess
type AllGuessProbability = [([Card], Int)]

-- Feedback for each guess
type AllGuessFeedback = [([Card], [(Int,Int,Int,Int,Int)])]

-- Calulate Feedback for each guess
calAllGuessFebk :: GameState ->GameState -> AllGuessFeedback
calAllGuessFebk [] _ = []
calAllGuessFebk (a:as) gs = (a, calAllGuessFebk' a gs) : calAllGuessFebk as gs

calAllGuessFebk' :: [Card] -> GameState -> [(Int,Int,Int,Int,Int)]
calAllGuessFebk' _ [] = []
calAllGuessFebk' card (elem:leftGameState) = (feedback card elem) : calAllGuessFebk' card leftGameState


-- calculate the mathematical expectation of each possible guess
calAllLeftProb :: AllGuessFeedback -> AllGuessProbability
calAllLeftProb [] = []
calAllLeftProb (x:xs) = (fst x, sum $ map (^2) $ map length $ group $ sort $ snd x) : calAllLeftProb xs
