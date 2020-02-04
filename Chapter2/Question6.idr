module Question6

export
counts : String -> (Nat, Nat)
counts s = let wordsCount = List.length (words s)
               inputLenght = Strings.length s in
               (wordsCount, inputLenght)

-- *Question6> counts "Hello, Idris world!"
-- (3, 19) : (Nat, Nat)