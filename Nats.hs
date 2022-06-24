{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Nats where
--import qualified Prelude
data Nat where
    Z :: Nat
    S :: Nat -> Nat
    --deriving (Prelude.Show)
data Bool where
    True :: Bool
    False :: Bool
    --deriving (Prelude.Show)
data List a where
    Empty' :: List a
    Cons' :: a -> List a -> List a
data ListNat where
    Empty :: ListNat
    Cons :: Nat -> ListNat -> ListNat
    --deriving (Prelude.Show)

ifthenelse :: Bool -> a -> a -> a
ifthenelse True x y = x
ifthenelse False x y = y

(==) :: Nat -> Nat -> Bool
Z == Z = True
S n == Z = False
Z == S n = False
S n == (S m) = n == m

(+) :: Nat -> Nat -> Nat
n + Z = n
n + (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
n * Z = Z
n * (S m) = (n * m) + n

(^) :: Nat -> Nat -> Nat
n ^ Z = S Z
n ^ (S m) = n ^ m * n

double :: Nat -> Nat
double Z = Z
double (S n) = S (S (double n))

pd :: Nat -> Nat
pd Z = Z
pd (S n) = n

(-) :: Nat -> Nat -> Nat
n - Z = n
n - (S m) = pd (n - m)

fact :: Nat -> Nat
fact Z = S Z
fact (S n) = fact n * S n

fib :: Nat -> Nat
fib Z = Z
fib (S Z) = S Z
fib (S (S n)) = fib (S n) + fib n

quot :: (Nat, Nat) -> Nat
quot (Z, n) = Z
quot (n, Z) = Z
quot (S n, S m) = quot (S n - S m, S m) + (S Z + (S m - S n))

rem :: (Nat, Nat) -> Nat
rem (Z, S n) = Z
rem (S n, Z) = Z
rem (Z, Z) = Z
rem (S n, S m) = S n - ( S m * quot (S n, S m))

mod :: Nat -> Nat -> Nat
mod m n = rem (n, m)

--div :: (Nat, Nat) -> (Nat, Nat)
--div (S n, S m) = (quot(S n, S m), rem(S n, S m))

--gcd
--lcm
--comb
--perm

and :: Bool -> Bool -> Bool
b `and` True = b
b `and` False = False

or :: Bool -> Bool -> Bool
False `or` False = False
b `or` c = True

not :: Bool -> Bool
not True = False
not False = True

xor :: Bool -> Bool -> Bool
False `xor` False = False
False `xor` True = True
True `xor` False = True
True `xor` True = False

leq :: Nat -> Nat -> Bool
Z `leq` n  = True
S m `leq` Z = False
S m `leq` S n = m `leq` n

od :: Nat -> Bool
od Z = False
od (S n) = ev n

ev :: Nat -> Bool
ev Z = True
ev (S n) = od n

length :: ListNat -> Nat
length Empty = Z
length (Cons n ns) = S (length ns)

elem :: Nat -> ListNat -> Bool
elem n Empty = False
elem n (Cons m ms) = ifthenelse (n == m) True (elem n ms)

sum :: ListNat -> Nat
sum Empty = Z
sum (Cons n ns) = n + sum ns

prod :: ListNat -> Nat
prod Empty = S Z
prod (Cons n ns) = n * prod ns

(++) :: ListNat -> ListNat -> ListNat
Empty ++ ns = ns
(Cons m ms) ++ ns = Cons m (ms ++ ns)

reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons n ns) = reverse ns ++ Cons n Empty

min :: Nat -> Nat -> Nat
min n Z = Z
min Z n = Z
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max n Z = n
max Z n = n
max (S n) (S m) = S (max n m)

allEven :: ListNat -> Bool
allEven Empty = True
allEven (Cons n ns) = ifthenelse (ev n) (allEven ns) False

anyEven :: ListNat -> Bool
anyEven Empty = False
anyEven (Cons n ns) = ifthenelse (ev n) True (anyEven ns)

allOdd :: ListNat -> Bool
allOdd Empty = True
allOdd (Cons n ns) = ifthenelse (od n) (allOdd ns) False

anyOdd :: ListNat -> Bool
anyOdd Empty = False
anyOdd (Cons n ns) = ifthenelse (od n) True (anyOdd ns)

isZero  :: Nat -> Bool
isZero Z = True
isZero (S n) = False

allZero :: ListNat -> Bool
allZero Empty = False
allZero (Cons n ns) = ifthenelse (n == Z) (allZero ns) False

anyZero :: ListNat -> Bool
anyZero Empty = False
anyZero (Cons n ns) = ifthenelse (isZero n) True (anyZero ns)

addNat :: Nat -> ListNat -> ListNat
addNat n Empty = Empty
addNat n (Cons m ms) = Cons (n + m) (addNat n ms)

multNat :: Nat -> ListNat -> ListNat
multNat n Empty = Empty
multNat n (Cons m ms) = Cons (n * m) (multNat n ms)

expNat :: Nat -> ListNat -> ListNat
expNat n Empty = Empty
expNat n (Cons m ms) = Cons (n ^ m) (expNat n ms)

--enumTo (S n) = reverse (Cons (S n) (enumTo n))
enumTo :: Nat -> ListNat
enumTo Z = Cons Z Empty
enumTo (S n) = enumTo n ++ Cons (S n) Empty

enumFromTo :: Nat -> Nat -> ListNat
enumFromTo Z Z = Cons Z Empty
enumFromTo Z m = enumTo m
enumFromTo n Z = reverse (enumTo n)
enumFromTo n m = ifthenelse (n `leq` m) (Cons n (enumFromTo (S n) m)) Empty

countdownFromTo :: Nat -> Nat -> ListNat
countdownFromTo Z Z = Cons Z Empty
countdownFromTo Z m = Empty
countdownFromTo (S n) m = ifthenelse (S n `leq` m) Empty (Cons (S n) (countdownFromTo n m))


--enumFromTo 3 6 -> 3:enumFromTo 4 6 -> 3:4:enumFromTo 5 6 -> 3:4:5:enumFromTo 6 6 -> 3:4:5:6:enumFromTo 7 6 ->
take :: Nat -> ListNat -> ListNat
take _ Empty = Empty
take Z _ = Empty
take (S n) (Cons x xs) = Cons x (take n xs)

drop :: Nat -> ListNat -> ListNat
drop _ Empty = Empty
drop Z ns = ns
drop (S n) (Cons x xs) = drop n xs

-- elemIndices :: Nat -> ListNat -> ListNat

pwAdd :: ListNat -> ListNat -> ListNat
pwAdd (Cons n ns) (Cons m ms) = Cons (n + m) (pwAdd ns ms)
pwAdd _ _ = Empty


pwMult :: ListNat -> ListNat -> ListNat
pwMult (Cons n ns) (Cons m ms) = Cons (n * m) (pwMult ns ms)
pwMult _ _ = Empty

pointwise :: (a -> a -> a) -> List a -> List a -> List a
pointwise f (Cons' n ns) (Cons' m ms) = Cons' (f n m) (pointwise f ns ms)
pointwise f _ _ = Empty'

filter :: (a -> Bool) -> List a -> List a
filter p (Cons' x xs) = ifthenelse (p x) (Cons' x (filter p xs)) (filter p xs)
filter p _ = Empty'


filterEven :: ListNat -> ListNat
filterEven Empty = Empty
filterEven (Cons n ns) = ifthenelse (ev n) (Cons n Empty ++ filterEven ns) (filterEven ns)

filterOdd :: ListNat -> ListNat
filterOdd Empty = Empty
filterOdd (Cons n ns) = ifthenelse (od n) (Cons n Empty ++ filterOdd ns) (filterOdd ns)

-- isSorted :: ListNat -> Bool
-- minimum :: ListNat -> Nat
-- maximum :: ListNat -> Nat

isPrefixOf :: ListNat -> ListNat -> Bool
isPrefixOf _ Empty = True
isPrefixOf Empty ms = False
isPrefixOf (Cons n ns) (Cons m ms) = ifthenelse (n == m) (isPrefixOf ns ms) False

-- mix :: ListNat -> ListNat -> ListNat
-- intersperse :: Nat -> ListNat -> ListNat

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile b Empty' = Empty'
takeWhile b (Cons' x xs) = ifthenelse (b x) (Cons' x (takeWhile b xs)) (Cons' x Empty')
--takeWhile b (Cons' x xs) = ifthenelse (b x) (Cons' x (takeWhile b xs)) (takeWhile b xs)

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile b Empty' = Empty'
dropWhile b (Cons' x xs) = ifthenelse (b x) (dropWhile b xs) (Cons' x xs)


--head :: List α -> Maybe α
--tail :: List α -> Maybe (List α)
--init :: List α -> Maybe (List α)
--last :: List α -> Maybe α
--pick :: Nat -> List α -> Maybe α

--findFirst :: α -> List α -> Maybe Nat

--find :: Nat -> List a -> ListNat
--find a Empty' = Empty
--find a (Cons' x xs) = ifthenelse (a == x) (Cons x (find a xs)) (find a xs)
