data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) = case takeN k xs of
                             Fewer => Fewer
                             (Exact n_xs) => Exact $ x :: n_xs

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

-- *Exercise10-1> groupByN 3 [1..10]
-- [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]] : List (List Integer)


halves : List a -> (List a, List a)
halves xs = splitHalf (div (length xs) 2) xs where
  splitHalf : Nat -> List a -> (List a, List a)
  splitHalf n xs with (takeN n xs)
    splitHalf n xs | Fewer = ([], xs)
    splitHalf n (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)

-- *Exercise10-1> halves [1..10]
-- ([1, 2, 3, 4, 5], [6, 7, 8, 9, 10]) : (List Integer, List Integer)

-- *Exercise10-1> halves [1]
-- ([], [1]) : (List Integer, List Integer)