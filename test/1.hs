data Suit = Club | Diamond | Heart | Spade
data Rank = R2 | R3 | R4 | R5 | R6 |R7 | R8 | R9 | R10
            | Jack | Queen | King | Ace
            deriving (Show,Eq,Ord)
data Card = Card Suit Rank
data JokerColor = Red | Black
data JCard = NormalCard Suit Rank | JokerCard JokerColor


data Mtree a = Mnode a [Mtree a]
showMtree :: Show a => Mtree a -> String
showMtree tree = showMtree' "" tree

showMtree' :: Show a => String -> Mtree a -> String
showMtree' indent (Mnode label subtrees) =
  indent ++ show label ++ "\n" ++ concatMap (showMtree' (' ':indent)) subtrees
  
  
merge :: Ord a = [a]->[a]->[a]
merge [][] = []
merge [] as = as
merge bs [] = bs

merge (a:as) (b:bs) 
    | a>=b = a:merge as (b:bs)
    | otherwise = b:merge (a:as) bs
    
