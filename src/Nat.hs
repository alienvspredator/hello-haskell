module Nat where

data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)

instance Num Nat where
    (+) a Zero     = a
    (+) a (Succ b) = Succ ((max a b) + (min a b))
    negate _ = error "Negate is undefined for Nat"
    (*) a Zero     = Zero
    (*) a (Succ b) = a + (a * b)
    abs a = a
    signum Zero = Zero
    signum _    = Succ Zero
    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n - 1))

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

beside :: Nat -> Nat -> Bool
beside a b = a == Succ b || b == Succ a

beside2 :: Nat -> Nat -> Bool
beside2 a b = a == Succ (b + 2) || b == Succ (a + 2)

pow :: Nat -> Nat -> Nat
pow a Zero     = 1
pow a (Succ b) = a * (pow a b)

pred :: Nat -> Nat
pred x = y where (Succ y) = x
-- pred x = let (Succ y) = x in y
