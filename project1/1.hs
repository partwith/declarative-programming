module test (feedback, initialGuess, nextGuess, GameState) where  
import Card.hs

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback (cardAns:answer) (cardGue:guess) 
    | cardAns == cardGue = (1,1,1,1,1)