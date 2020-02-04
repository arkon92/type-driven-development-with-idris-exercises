import Data.Vect

data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
     Unicycle : Vehicle Pedal
     Bicycle : Vehicle Pedal
     ElectricCar : (battery : Nat) -> Vehicle Electricity
     Tram : (battery : Nat) -> Vehicle Electricity
     Car : (fuel : Nat) -> Vehicle Petrol
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels ElectricCar = 4
wheels Tram = 6
wheels (Car fuel) = 4
wheels (Motorcycle fuel) = 2
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel Unicycle impossible
refuel Bicycle impossible
refuel (Car fuel) = Car 100
refuel (Motorcycle fuel) = Car 50
refuel (Bus fuel) = Bus 200


vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z _ = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

-- *Exercise4-2> vectTake 3 [1, 2, 3, 4, 5, 6, 7]
-- [1, 2, 3] : Vect 3 Integer

{-
*Exercise4-2> vectTake 8 [1, 2, 3, 4, 5, 6, 7]
(input):1:13:When checking argument xs to constructor Data.Vect.:::
        Type mismatch between
                Vect 0 elem (Type of [])
        and
                Vect (S m) a (Expected type)

        Specifically:
                Type mismatch between
                        0
                and
                        S m
-}


sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos {n} x y = case integerToFin pos n of
                          Nothing => Nothing
                          Just fin => Just ((Vect.index fin x) + (Vect.index fin y))

-- *Exercise4-2> sumEntries 2 [1, 2, 3, 4] [5, 6, 7, 8]
-- Just 10 : Maybe Integer

-- *Exercise4-2> sumEntries 4 [1, 2, 3, 4] [5, 6, 7, 8]
-- Nothing : Maybe Integer