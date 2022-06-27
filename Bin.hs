{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Bin where

data Nat where
    Z :: Nat
    S :: Nat -> Nat
data Bin where
    O :: Bin
    I :: Bin
    BZ :: Bin -> Bin
    BI :: Bin -> Bin

(+) :: Nat -> Nat -> Nat
n + Z = n
n + (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
n * Z = Z
n * (S m) = (n * m) + n

eval :: Bin -> Nat
eval O = Z
eval I = S Z
eval (BZ b) = S (S Z) * eval b
eval (BI b) = S (S (S Z) * eval b)