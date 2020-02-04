over_length : Nat -> List String -> Nat
over_length s l = length (filter (\x => length x > s) l)

-- *Question8> over_length 3 ["One", "Two", "Three", "Four"]
-- 2 : Nat