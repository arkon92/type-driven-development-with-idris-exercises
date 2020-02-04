module Question3

export
palindrome : String -> Bool
palindrome s = let lower = toLower s
                   reversed = reverse lower in
                   lower == reversed

-- *Question3> palindrome "Racecar"
-- True : Bool