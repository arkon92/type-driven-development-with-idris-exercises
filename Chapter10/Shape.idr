module Shape

public export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

public export
triangle : Double -> Double -> Shape
triangle = Triangle

public export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

public export
circle : Double -> Shape
circle = Circle

public export
data ShapeView : (s : Shape) -> Type where
     STriangle : ShapeView (Triangle base height)
     SRectangle : ShapeView (Rectangle width height)
     SCircle : ShapeView (Circle r)

public export
shapeView : (s : Shape) -> ShapeView s
shapeView (Triangle x y) = STriangle
shapeView (Rectangle x y) = SRectangle
shapeView (Circle x) = SCircle