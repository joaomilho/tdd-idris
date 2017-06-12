topTen : Ord a => List a -> List a
topTen = List.take 10 . reverse . sort
