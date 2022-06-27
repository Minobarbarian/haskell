{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Order where
    
data Nat where
    Z :: Nat
    S :: Nat -> Nat
data Order where
    LT :: Order
    EQ :: Order
    GT :: Order

compare :: Nat -> Nat -> Order
compare Z Z = EQ
compare Z (S m) = LT
compare (S n) Z = GT
compare (S n) (S m) = compare n m