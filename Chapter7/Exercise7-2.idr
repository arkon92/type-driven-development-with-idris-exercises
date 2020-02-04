data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Neg num, Integral num, Abs num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs


Show ty => Show (Expr ty) where
     show (Val x) = show x
     show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
     show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
     show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
     show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
     show (Abs x) = "|" ++ show x ++ "|"

-- *Exercise7-2> show ((the (Expr _)) (6 + 3 * 12))
-- "(6 + (3 * 12))" : String

-- *Exercise7-2> show ((the (Expr _)) (6 + 3 + 12))
-- "((6 + 3) + 12)" : String


(Eq ty, Neg ty, Integral ty, Abs ty) => Eq (Expr ty) where
   (==) x y = eval x == eval y

-- *Exercise7-2> the (Expr _) (2 + 4) == 3 + 3
-- True : Bool

-- *Exercise7-2> the (Expr _) (2 + 4) == 3 + 4
-- False : Bool


(Neg ty, Integral ty, Abs ty) => Cast (Expr ty) ty where
  cast x = eval x

-- *Exercise7-2> let x : Expr Integer = 6 * 3 + 12 in the Integer (cast x)
-- 30 : Integer
