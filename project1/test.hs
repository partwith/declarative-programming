import Card

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] [] = (0,0,0,0,0) 
feedback answer guess
    =  (eqNum answer guess,
        lowNum answer guess,
        sameRank answer guess,
        highNum answer guess,
        sameSuit answer guess)
        
eqNum :: [Card] -> [Card] -> Int   
eqNum [] _ = 0
eqNum (card:answer) guess = 
    compareCard card guess + eqNum answer guess
    
compareCard :: Card -> [Card] -> Int
compareCard _ [] = 0
compareCard card (cardGuess:guess)
    | card == cardGuess = 1
    | otherwise = compareCard card guess

lowNum :: [Card] -> [Card] -> Int 
lowNum [] _ = 0
lowNum ((Card s1 r1):answer) guess =
    let Card s2 r2 = lowestCard guess in 
    if r1 < r2 then 1 + lowNum answer guess 
    else lowNum answer guess
    
lowCard :: Card -> Card -> Card
lowCard (Card s1 r1) (Card s2 r2) =
    let rankOrder = compare r1 r2 in
    if rankOrder == LT then Card s1 r1 else Card s2 r2

lowestCard :: [Card] -> Card 
lowestCard (card1:card2:guess) =
    let minCard = lowCard card1 card2 in
    if length guess == 0 then minCard
    else 
        if minCard == card1 then lowestCard (card1:guess) 
        else lowestCard (card2:guess)

highNum :: [Card] -> [Card] -> Int 
highNum [] _ = 0
highNum ((Card s1 r1):answer) guess =
    let Card s2 r2 = highestCard guess in 
    if r1 > r2 then 1 + highNum answer guess 
    else highNum answer guess
    
highCard :: Card -> Card -> Card
highCard (Card s1 r1) (Card s2 r2) =
    let rankOrder = compare r1 r2 in
    if rankOrder == GT then Card s1 r1 else Card s2 r2

highestCard :: [Card] -> Card 
highestCard (card1:card2:guess) =
    let minCard = highCard card1 card2 in
    if length guess == 0 then minCard
    else 
        if minCard == card1 then highestCard (card1:guess) 
        else highestCard (card2:guess)
            
sameRank :: [Card] -> [Card] -> Int 
sameRank [] _ = 0
sameRank (card:answer) guess = 
    compareRank card guess + sameRank answer guess

compareRank :: Card -> [Card] -> Int
compareRank _ [] = 0
compareRank (Card s1 r1) ((Card s2 r2) : guess)
    | r1 == r2 = 1
    | otherwise = compareRank (Card s1 r1) guess

sameSuit :: [Card] -> [Card] -> Int 
sameSuit [] _ = 0
sameSuit (card:answer) guess = 
    compareSuit card guess + sameSuit answer guess

compareSuit :: Card -> [Card] -> Int
compareSuit _ [] = 0
compareSuit (Card s1 r1) ((Card s2 r2) : guess)
    | s1 == s2 = 1
    | otherwise = compareSuit (Card s1 r1) guess