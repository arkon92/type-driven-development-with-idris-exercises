import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

-- *Exercise3-3-2> transposeMat [[1,2], [3,4], [5,6]]
-- [[1, 3, 5], [2, 4, 6]] : Vect 2 (Vect 3 Integer)


addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys

-- *Exercise3-3-2> addMatrix [[1,2], [3,4]] [[5,6], [7,8]]
-- [[6, 8], [10, 12]] : Vect 2 (Vect 2 Integer)


computeElement : Num a => Vect n a -> Vect n a -> a
computeElement x y = sum (Data.Vect.zipWith (*) x y)

computeRow : Num a => Vect n a -> Vect m (Vect n a) -> Vect m a
computeRow x [] = Nil
computeRow x (y :: ys) = let currentElement = computeElement x y in
                             [currentElement] ++ computeRow x ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect o a) -> Vect n (Vect o a)
multMatrix [] y = []
multMatrix (x :: xs) y = let transposeY = transposeMat y
                             currentRow = computeRow x transposeY in
                             [currentRow] ++ multMatrix xs y

-- Exercise3-3-2> multMatrix [[1,2], [3,4], [5,6]] [[7,8,9,10], [11,12,13,14]]
-- [[29, 32, 35, 38], [65, 72, 79, 86], [101, 112, 123, 134]] : Vect 3 (Vect 4 Integer)

