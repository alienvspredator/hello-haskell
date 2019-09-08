module Lib
    ( someFunc
    )
where

someFunc :: IO ()
someFunc = do
    putStr "Enter the number:"
    n <- readInt
    print (fac n)

readInt :: IO Int
readInt = readLn

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)
