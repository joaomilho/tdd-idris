module Planets

import Data.Vect

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune
-- deriving Eq

planets : Vect 8 Planet
planets = [Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune]

indexOf : Planet -> Nat
indexOf Mercury = 0
indexOf Venus = 1
indexOf Earth = 2
indexOf Mars = 3
indexOf Jupiter = 4
indexOf Saturn = 5
indexOf Uranus = 6
indexOf Neptune = 7

closestToSun : Planet -> Planet -> Planet
closestToSun p1 p2 = if indexOf p1 < indexOf p2 then p1 else p2

inSeq : Planet -> Planet -> Bool
inSeq p1 p2 = S (indexOf p1) == indexOf p2
