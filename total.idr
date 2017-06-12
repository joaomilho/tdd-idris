import Data.Vect

total count : List a -> Nat
count [] = Z
count (x::xs) = S (count xs)


total c5 : Vect i a -> Nat
c5 {i=Z} _ = 0
c5 {i=S k} (x::xs) = S (c5 xs)
