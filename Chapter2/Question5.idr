palindrome : Nat -> String -> Bool
palindrome l s = let lower = toLower s
                     reversed = reverse lower in
                     if length lower > l then lower == reversed else False

-- *Question5> palindrome 10 "racecar"
-- False : Bool
-- *Question5> palindrome 5 "racecar"
-- True : Bool