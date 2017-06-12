module Expressions

data Expr = Val Nat | Add Expr Expr | Mult Expr Expr


evaluate : Expr -> Nat
evaluate (Val v) = v
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Mult a b) = evaluate a * evaluate b

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe x Nothing = x
maxMaybe Nothing x = x
maxMaybe (Just a) (Just b) = Just (max a b)
