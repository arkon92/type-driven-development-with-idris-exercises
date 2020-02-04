import DataStore
import Shape

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues x with (storeView x)
  getValues x | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) = value :: getValues store | rec

-- *Exercise10-3> getValues testStore
-- [1, 2] : List Int


area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = 0.5 * base * height
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius

-- *Exercise10-3> area (triangle 3 4)
-- 6.0 : Double

-- *Exercise10-3> area (circle 10)
-- 314.1592653589793 : Double