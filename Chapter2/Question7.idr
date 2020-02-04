top_ten : Ord a => List a -> List a
top_ten l = take 10 (reverse (sort l))

-- *Question7> top_ten [1..100]
-- [100, 99, 98, 97, 96, 95, 94, 93, 92, 91] : List Integer