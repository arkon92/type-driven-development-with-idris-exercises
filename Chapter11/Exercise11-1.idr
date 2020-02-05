import Data.Primitives.Views

every_other : Stream Integer -> Stream Integer
every_other (x :: y :: xs) = y :: every_other xs

-- *Exercise11-1> take 10 (every_other [1..])
-- [2, 3, 4, 5, 6, 7, 8, 9, 10, 11] : List Integer


data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : Nat -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (x :: xs) = x :: getPrefix k xs

Functor InfList where
  map func (x :: xs) = func x :: map func xs

-- *Exercise11-1> getPrefix 10 (map (*2) (countFrom 1))
-- [2, 4, 6, 8, 10, 12, 14, 16, 18, 20] : List Integer


data Face : Type where
     Heads : Face
     Tails : Face

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z (value :: xs) = []
coinFlips (S k) (value :: xs) = getFace value :: coinFlips k xs
  where getFace : Int -> Face
        getFace x with (divides x 2)
          getFace ((2 * div) + rem) | (DivBy prf) = if rem == 0 then Heads else Tails

-- *Exercise11-1> coinFlips 6 (randoms 12345)
-- [Tails, Heads, Tails, Tails, Heads, Tails] : List Face


square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx =
  let next = (approx + (number / approx)) / 2 in
      next :: square_root_approx number next

-- *Exercise11-1> take 3 (square_root_approx 10 10)
-- [5.5, 3.659090909090909, 3.196005081874647] : List Double

-- *Exercise11-1> take 3 (square_root_approx 100 25)
-- [14.5, 10.698275862068964, 10.022788213065104] : List Double


square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: vs) = value
square_root_bound (S k) number bound (v :: vs) =
  case (v * v) < bound of
       True => v
       False => square_root_bound k number bound vs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.0000000001 (square_root_approx number number)

-- *Exercise11-1> square_root 6
-- 2.449489742783178 : Double

-- *Exercise11-1> square_root 2500
-- 50.0 : Double

-- *Exercise11-1> square_root 2501
-- 50.009999000199954 : Double