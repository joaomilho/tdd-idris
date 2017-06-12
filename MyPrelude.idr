module MyPrelude

import Data.Vect

myMap : (a -> b) -> Vect l a -> Vect l b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

myReverse : List a -> List a
myReverse [] = []
myReverse (x::xs) = myReverse xs ++ [x]

myHead : Vect (S len) elem -> Vect 1 elem
myHead (x::xs) = [x]

myFlip : Vect n Int -> Vect (n + 1) Int
myFlip {n = Z} x = x ++ [1]
myFlip {n = (S k)} x = x ++ [head x]
