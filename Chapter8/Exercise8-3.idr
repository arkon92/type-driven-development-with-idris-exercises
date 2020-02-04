data Vect : Nat -> (elem : Type) -> Type where
  Nil : Vect Z elem
  (::) : elem -> Vect n elem -> Vect (S n) elem


headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

-- *Exercise8-3> :total headUnequal
-- Main.headUnequal is Total


tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

-- *Exercise8-3> :total tailUnequal
-- Main.tailUnequal is Total


DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq x y of
                                 (Yes prfHead) => case decEq xs ys of
                                                   (Yes prfTail) => Yes (rewrite prfHead in rewrite prfTail in Refl)
                                                   (No contraTail) => No (tailUnequal contraTail)
                                 (No contraHead) => No $ headUnequal contraHead

-- *Exercise8-3> decEq (the (Vect _ _) [1, 2, 3]) [1, 2, 3]
-- Yes Refl : Dec ([1, 2, 3] = [1, 2, 3])