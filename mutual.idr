mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k


  isSucc : Nat -> Nat -> Bool
  isSucc Z _ = False
  isSucc (S a) b = if a == b then True else isSucc a b

  isBigger : Nat -> Nat -> Bool
  isBigger = isSucc

  isAnt : Nat -> Nat -> Bool
  isAnt _ Z = False
  isAnt a (S b) = if a == b then True else isAnt a b

  isLower : Nat -> Nat -> Bool
  isLower = isAnt

  isSame : Nat -> Nat -> Bool
  isSame Z Z = True
  isSame Z _ = False
  isSame _ Z = False
  isSame (S a) (S b) = isSame a b
