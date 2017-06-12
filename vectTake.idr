import Data.Vect

vectTake : (m : Fin (S n)) -> Vect n a -> Vect (cast m) a
vectTake FZ _ = []
vectTake (FS k) (x :: xs) = x :: vectTake k xs

-- usage
-- let vect = the (Vect 3 Int) [1,2,3] in vectTake 2 vect
-- or simply
-- vectTake 2 [1,2,3]
