module BinTree where

import Nat

data BinTree a = List a | BinTree (BinTree a, BinTree a)
    deriving (Show, Eq, Ord)

reverseBinTree :: BinTree a -> BinTree a
reverseBinTree (BinTree (l, r)) = BinTree (r, l)

depth :: BinTree a -> Nat
depth (List    a     ) = Succ Zero
depth (BinTree (l, r)) = Succ (max (depth l) (depth r))

leaves :: BinTree a -> [a]
leaves (List a) = [a]
leaves (BinTree (l, r)) = (leaves l) ++ (leaves r)

-- pushLeft :: BinTree a -> BinTree a -> BinTree a
-- pushLeft (BinTree (List a, r)) b = BinTree (BinTree (List a, b), r)

-- pushRight :: BinTree a -> BinTree a -> BinTree a
-- pushRight (BinTree (l, List a)) b = BinTree (l, BinTree (List a, b))
