--  File     : Assignment1.hs
--  Author   : Jizhe Hou
--  Purpose  : Assignment1 project
--  Funtion  : 1. elementPosition : takes an element elt and a list lst, and returns the number of
--                                  elements of lst that must be skipped over to find the first 
--                                  occurrence of the of elt.
--             2. everyNth : takes a number n and a list lst and returns a list of every nth element of
--                           beginning with the nth element of lst.
--             3. elementBefore : takes an element elt and a list lst, and returns Just the element
--                                immediately before the first occurrence of elt in lst, if it exists.
--  Date     :  18/08/2016

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