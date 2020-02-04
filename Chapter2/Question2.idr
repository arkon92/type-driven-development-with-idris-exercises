palindrome : String -> Bool
palindrome s = let reversed = reverse s in
                   s == reversed

-- *Question2> palindrome "racecar"
-- True : Bool
-- *Question2> palindrome "race car"
-- False : Bool