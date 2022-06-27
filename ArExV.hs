{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module ArExV where
    
data Nat where
    Z :: Nat
    S :: Nat -> Nat
data ArExV where
    Lit :: Nat -> ArExV
    Plus :: ArExV -> ArExV -> ArExV
    Times :: ArExV -> ArExV -> ArExV
    Var :: Nat -> ArExV

(+) :: Nat -> Nat -> Nat
n + Z = n
n + (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
n * Z = Z
n * (S m) = (n * m) + n

eval :: (Nat -> Nat) -> ArExV -> Nat
eval v (Lit n) = n
eval v (Var n) = v n
eval v (Plus f g) = eval v f + eval v g
eval v (Times f g) = eval v f * eval v g