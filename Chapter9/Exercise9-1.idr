data ListElem : a -> List a -> Type where
  ListHere : ListElem x (x :: xs)
  ListThere : (later : ListElem x xs) -> ListElem x (y :: xs)


data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

isLastNil : Last [] value -> Void
isLastNil LastOne impossible
isLastNil (LastCons _) impossible

notLast : (notHere : (x = value) -> Void) -> Last [x] value -> Void
notLast prf LastOne = absurd (prf Refl)
notLast prf (LastCons _) impossible

notInTail : (contra : Last (y :: ys) value -> Void) -> Last (x :: y :: ys) value -> Void
notInTail contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No isLastNil
isLast (x :: []) value = case decEq x value of
                              Yes Refl => Yes LastOne
                              No notEq => No (notLast notEq)
isLast (x :: y :: xs) value = case isLast (y :: xs) value of
                                   Yes prf => Yes (LastCons prf)
                                   No c => No (notInTail c)

-- *Exercise9-1> isLast [1, 2, 3] 3
-- Yes (LastCons (LastCons LastOne)) : Dec (Last [1, 2, 3] 3)