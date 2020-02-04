data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (==) (Triangle x y) (Triangle x' y') = x == x' && y == y'
  (==) (Rectangle x y) (Rectangle x' y') = x == x' && y == y'
  (==) (Circle x) (Circle x') = x == x'
  (==) _ _ = False


area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

Ord Shape where
  compare s1 s2 = let area1 = area s1
                      area2 = area s2 in
                      compare area1 area2

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]

-- *Exercise7-1> sort testShapes
-- [Rectangle 2.0 6.0, Triangle 3.0 9.0, Rectangle 2.0 7.0, Circle 3.0, Circle 4.0] : List Shape
