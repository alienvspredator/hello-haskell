module Expression
    ( square
    )
where

square :: Floating a => a -> a -> a -> a
square a b c =
    let p = (a + b + c) / 2 in sqrt (p * (p - a) * (p - b) * (p - c))

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []       = []
filter' p (x : xs) = let rest = filter' p xs in if p x then x : rest else rest

all' :: (a -> Bool) -> [a] -> Bool
all' p []       = True
all' p (x : xs) = p x && all' p xs
-- all' p (x : xs) = if p x then all' p xs else False
