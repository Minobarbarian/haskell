{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module PL where

data Nat where
    Z :: Nat
    S :: Nat -> Nat
data Bool where
    True :: Bool
    False :: Bool
data PL where 
    Pvar :: Nat -> PL
    Neg :: PL -> PL
    Con :: PL -> PL -> PL
    Dis :: PL -> PL -> PL

and :: Bool -> Bool -> Bool
and b True = b
and b False = False

or :: Bool -> Bool -> Bool
or b True = True
or b False = b

not :: Bool -> Bool
not True = False
not False = True

sneg :: PL -> PL
sneg (Pvar n) = Pvar n
sneg (Neg (Pvar n)) = Neg (Pvar n)
sneg (Neg (Neg f)) = sneg f
sneg (Neg (Con f g)) = Dis (sneg (Neg f)) (sneg (Neg g))
sneg (Neg (Dis f g)) = Con (sneg (Neg f)) (sneg (Neg g))
sneg (Con f g) = Con (sneg f) (sneg g)
sneg (Dis f g) = Dis (sneg f) (sneg g)

eval :: (Nat -> Bool) -> PL -> Bool
eval v (Pvar n) = v n
eval v (Neg f) = not (eval v f)
eval v (Con f g) = and (eval v f) (eval v g)
eval v (Dis f g) = or (eval v f) (eval v g)
