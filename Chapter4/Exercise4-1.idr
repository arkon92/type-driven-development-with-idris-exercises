data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = let intialTree = insert x Empty in
                           foldl (\acc,y => insert y acc) intialTree xs

-- *Exercise4-1> listToTree [1, 4, 3, 5, 2]
-- Node Empty 1 (Node (Node (Node Empty 2 Empty) 3 Empty) 4 (Node Empty 5 Empty)) : Tree Integer


treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node Empty val Empty) = [val]
treeToList (Node left val right) = (treeToList left) ++ [val] ++ (treeToList right)

-- *Exercise4-1> treeToList (listToTree [4, 1, 8, 7, 2, 3, 9, 5, 6])
-- [1, 2, 3, 4, 5, 6, 7, 8, 9] : List Integer


data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)

-- *Exercise4-1> evaluate (Mult (Val 10) (Add (Val 6) (Val 3)))
-- 90 : Int


maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe x Nothing = x
maxMaybe Nothing x = x
maxMaybe x y = case compare x y of
               LT => y
               EQ => x
               GT => x

-- *Exercise4-1> maxMaybe (Just 4) (Just 5)
-- Just 5 : Maybe Integer

-- *Exercise4-1> maxMaybe (Just 4) Nothing
-- Just 4 : Maybe Integer


data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive triangle@(Triangle x y)) = Just (area triangle)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate _ x) = biggestTriangle x
biggestTriangle (Translate _ _ x) = biggestTriangle x

-- *Exercise4-1> biggestTriangle testPic1
-- Just 4.0 : Maybe Double

-- *Exercise4-1> biggestTriangle testPic2
-- Nothing : Maybe Double