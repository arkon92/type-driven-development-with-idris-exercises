palindrome : String -> Bool
palindrome s = let lower = toLower s
                   reversed = reverse lower in
                   if length lower > 10 then lower == reversed else False

-- Question4> palindrome "racecar"
-- False : Bool
-- *Question4> palindrome "able was i ere i saw elba"
-- True : Bool