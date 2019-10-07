module Declarative
    ( square
    )
where

square :: Floating a => a -> a -> a -> a
square a b c = sqrt (p * (p - a) * (p - b) * (p - c))
    where p = (a + b + c) / 2

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs) | p x       = x : rest
                   | otherwise = rest
    where rest = filter' p xs

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x : xs) | p x       = all' p xs
                | otherwise = False
