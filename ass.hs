module Assignment1 (elementPosition, everyNth, elementBefore) where

elementPosition :: Eq t => t -> [t] -> Int
elementPosition elt [] = error "error, there does not have this element"
elementPosition elt (e:es) 
    | elt == e = 0 
    | otherwise = 1 + elementPosition elt es

everyNth :: Int -> [t] -> [t]
everyNth 0 _ = error "the span length can not be 0"
everyNth _ [] = []
everyNth n es
    | length es < n = [] 
    | otherwise = head (drop (n-1) es) : everyNth n (drop n es)
    
elementBefore :: Eq a => a -> [a] -> Maybe a
elementBefore elt (e:es)
    | length es == 0 = Nothing
    | head es == elt = Just e
    | otherwise = elementBefore elt es