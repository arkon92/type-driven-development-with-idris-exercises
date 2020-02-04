import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

-- *Exercise2-3> my_length [1..10]
-- 10 : Nat


my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = (my_reverse xs) ++ [x]

-- *Exercise2-3> my_reverse [1..10]
-- [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] : List Integer


my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

-- *Exercise2-3> my_map (* 2) [1..10]
-- [2, 4, 6, 8, 10, 12, 14, 16, 18, 20] : List Integer


my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

-- *Exercise2-3> my_vect_map length ["Hot", "Dog", "Jumping", "Frog"]
-- [3, 3, 7, 4] : Vect 4 Nat