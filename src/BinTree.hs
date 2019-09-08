module BinTree where

import Nat

data BinTree a = List a | BinTree (BinTree a, BinTree a)
    deriving (Show, Eq, Ord)

reverseBinTree :: BinTree a -> BinTree a
reverseBinTree (BinTree (l, r)) = BinTree (r, l)

depth :: BinTree a -> Nat
depth (List    a     ) = Succ Zero
depth (BinTree (l, r)) = Succ (max (depth l) (depth r))
