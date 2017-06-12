overLength : Nat -> List String -> Nat
overLength n =
    length
  . filter (>n)
  . map length
