module Main

import Data.Vect

data Tree = Leaf Char | Node Nat Tree Tree

l : Char -> Tree
l c = Leaf c

n : Nat -> Tree -> Tree -> Tree
n i t1 t2 = Node i t1 t2

tree : Tree
tree = n 0 (n 1 (l 'a') (l 'b')) (n 2 (l 'c') (n 3 (l 'd') (l 'e')))

repeat : String -> Nat -> String
repeat _ Z = ""
repeat s (S Z) = s
repeat s (S n) = s ++ (repeat s n)

mutual
  toS : Tree -> String
  toS (Leaf s) = cast s
  toS (Node n t1 t2) = show n ++ "->" ++ (toS' t1 1) ++ "\n|->" ++ (toS' t2 1)

  toS' : Tree -> Nat -> String
  toS' (Leaf s) _ = cast s
  toS' (Node n t1 t2) idx = show n ++ "->" ++ (toS' t1 (S idx)) ++ "\n" ++ (repeat " " (idx * 3)) ++ "|->" ++ (toS' t2 (S idx))

main : IO ()
main = putStrLn $ toS tree
