module Palindrome

palindrome : String -> Bool
palindrome x =
  x == reverse x

lenMinus : String -> Integer -> Nat
lenMinus s n = toNat $ toIntegerNat (length s) - n

-- cast is necessary because length is Nat, so -1 may not work
last : String -> Char
last s = strIndex s (cast (lenMinus s  1))

kernel : String -> String
kernel s = substr 1 (lenMinus s 2) s

palindromeR : String -> Bool
palindromeR s =
  case (length s) of
    Z => True
    S Z => True
    _ => if strHead s == last s then palindromeR (kernel s) else False
