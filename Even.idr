module Even

%elim data Even =
  ||| Mah?
  Z |
  ||| Od?
  S Even

%name Even k,j,i,n,m

toInt : Even -> Integer
toInt Even.Z = 0
toInt (Even.S k) = 2 + toInt k


||| It's something!
asN : Even -> Nat
asN = fromIntegerNat . toInt

fromIntegerEven : Integer -> Even
fromIntegerEven 0 = Z
fromIntegerEven n =
  if (n > 0) then
    S (fromIntegerEven (assert_smaller n (n - 1)))
  else
    Z

--------------------------------------------------------------------------------
-- Ops
--------------------------------------------------------------------------------

||| Add two even numbers.
||| @ n the number to case-split on
||| @ m the other number
total plusE : (n, m : Even) -> Even
plusE Z right        = right
plusE (S left) right = S (plusE left right)

||| Multiply even numbers
total multE : Even -> Even -> Even
multE Z right        = Z
multE (S Z)    right = plusE right right
multE (S left) right = plusE right $ multE left right


--------------------------------------------------------------------------------
-- Interface implementations
--------------------------------------------------------------------------------

-- Eq Nat where
--   Z == Z         = True
--   (S l) == (S r) = l == r
--   _ == _         = False

-- Ord Nat where
--   compare Z Z         = EQ
--   compare Z (S k)     = LT
--   compare (S k) Z     = GT
--   compare (S x) (S y) = compare x y

Num Even where
  (+) = plusE
  (*) = multE

  fromInteger = fromIntegerEven

Cast Integer Even where
  cast = fromInteger

Cast Even Integer where
  cast = toInt

-- Cast String Even where
--    cast str = cast (the Integer (cast str))
