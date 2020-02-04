data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs


Functor Expr where
        map func (Val x) = Val (func x)
        map func (Add x y) = Add (map func x) (map func y)
        map func (Sub x y) = Sub (map func x) (map func y)
        map func (Mul x y) = Mul (map func x) (map func y)
        map func (Div x y) = Div (map func x) (map func y)
        map func (Abs x) = Abs (map func x)

-- *Exercise7-3> map (*2) (the (Expr _) (1 + 2 * 3))
-- Add (Val 2) (Mul (Val 4) (Val 6)) : Expr Integer

-- *Exercise7-3> map show (the (Expr _) (1 + 2 * 3))
-- Add (Val "1") (Mul (Val "2") (Val "3")) : Expr String


data Vect : (size : Nat) -> (ty : Type) -> Type where
  Nil : Vect Z ty
  (::) : ty -> Vect size ty -> Vect (S size) ty

Eq ty => Eq (Vect n ty) where
   (==) [] [] = True
   (==) (x :: xs) (y :: ys) = (x == y) && (xs == ys)

Foldable (Vect n) where
  foldr func acc [] = acc
  foldr func acc (x :: xs) = func x (foldr func acc xs)

-- *Exercise7-3> foldr (+) 0 (the (Vect _ _) [1,2,3,4,5])
-- 15 : Integer

-- *Exercise7-3> the (Vect _ _) [1,2,3,4] == [1,2,3,4]
-- True : Bool

-- *Exercise7-3> the (Vect _ _) [1,2,3,4] == [5,6,7,8]
-- False : Bool