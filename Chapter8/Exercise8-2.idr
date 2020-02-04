import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = let c = myPlusCommutes k m in
                             rewrite c in rewrite (plusSuccRightSucc m k) in Refl


reverseProof_nil : Vect n a -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect ((S n) + len) a -> Vect (plus n (S len)) a
reverseProof_xs {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs) = reverseProof_xs (reverse' (x :: acc) xs)

-- *Exercise8-2> myReverse [1, 2, 3, 4]
-- [4, 3, 2, 1] : Vect 4 Integer