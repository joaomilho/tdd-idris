import Data.Vect

biggerThanOne : Num a => Ord a => Vect l a -> Vect l Bool
biggerThanOne xs = map (>1) xs
