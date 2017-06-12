-- Ord a =>
data Tree = Empty | Node Tree Nat Tree

tree : Tree
tree = Node Empty 0 Empty

-- Ord elem =>
insert : Tree -> Nat -> Tree
insert Empty x  = Node Empty x Empty
insert (Node left val right) x = case compare x val of
                                      LT => Node (insert left x) val right
                                      EQ => Node left val right
                                      GT => Node left val (insert right x)

-- mutual
--   listToTree : List Nat -> Tree
--   listToTree [] = Empty
--   listToTree xs = listToTree' xs Empty
--
--   listToTree' : List Nat -> Tree -> Tree
--   listToTree' [] t = t
--   listToTree' (x::xs) t = listToTree' xs tree where
--     tree = (insert t x)
--
-- Ord a =>
listToTree : List Nat -> Tree
listToTree lst = foldl insert Empty lst

treeToList : Tree -> List Nat
treeToList Empty = []
treeToList (Node left val right) = val :: (treeToList left) ++ (treeToList right)

-- listToTree [1,4,3,5,2]
