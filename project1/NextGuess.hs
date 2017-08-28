--  File     : NextGuess.hs
--  Author   : Cai Chen
--  Purpose  : An implementation of nextGuess function

-- | This code generates the next guess and gamestate based on the 
--   previous guess and gamestate. 

module NextGuess (nextGuess) where

import Data.List
import Data.Function (on)
import Card
import GameState
import Feedback

-- | The main function of nextGuess
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int)->([Card],GameState)
nextGuess (c, gs) (cc, lr, cr, hr, cs) 
                -- | If the number of correct cards equals the length of guess, the guesser gets right answers
                -- | Otherwiese, the guesser should prepare next guess
                | cc == length c = (c, [c])
                | otherwise      = (newC, newGs)
                where
                    newC 
                        -- | Defining the threshold of 1000 for 3 and 4 cards that when the answers of the gamestate is
                        -- | more the threshold, it just chooses the first card lists as the next guess, otherwise determines 
                        -- | the next guess by calculating the probalility of answers left of each answer
                        | length c ==4 && length newGs > 1000  = newGs !! 0
                        | length c ==4 && length newGs <= 1000 = deterNextGuess newGs
                        | otherwise                            = deterNextGuess newGs 
                    -- | Remove the wrong answers from the gamestate according to the previous guess
                    newGs = filterByFeedback gs c (cc, lr, cr, hr, cs)

                    
-- Filter the remaing possible answers with same feedback as the input
filterByFeedback :: GameState -> [Card] -> (Int,Int,Int,Int,Int) -> GameState
filterByFeedback gs c f = filter (\x -> (feedback x c) == f) gs 




-- Determin next guess with the remaining possible answers by choosing the one with
-- probalility of least cards left for next guess
deterNextGuess :: GameState -> [Card]
deterNextGuess gs = fst $ head $ sortBy (compare `on` snd) $ calCardsLeftProb $ calGuessFebk gs gs 

-- Probability of cards left for the guess
type GuessProb = [([Card], Int)]

-- Feedback for each guess
type GuessFebk = [([Card], [(Int,Int,Int,Int,Int)])]

-- Calulate Feedback for each guess
calGuessFebk :: GameState ->GameState -> GuessFebk
calGuessFebk [] _ = []
calGuessFebk (a:as) gs = (a, calGuessFebk' a gs) : calGuessFebk as gs

calGuessFebk' :: [Card] -> GameState -> [(Int,Int,Int,Int,Int)]
calGuessFebk' _ [] = []
calGuessFebk' c (x:xs) = (feedback c x) : calGuessFebk' c xs

-- Calulate probalility for each guess
calCardsLeftProb :: GuessFebk -> GuessProb
calCardsLeftProb [] = []
calCardsLeftProb (x:xs) = (fst x, sum $ map (^2) $ map length $ group $ sort $ snd x) : calCardsLeftProb xs



