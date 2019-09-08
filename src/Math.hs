module Math
    ( fac
    , fib
    )
where

fac :: (Integral a) => a -> a
fac n = product [1..n]
-- fac 0 = 1
-- fac n = toInteger n * fac (n - 1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib n = fibs!!n
