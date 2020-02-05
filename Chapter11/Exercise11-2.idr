data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run Dry y = putStrLn "Out of fuel."
run (More x) (Do y f) = do res <- y
                           run x (f res)

forever : Fuel
forever = More forever

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do putStr prompt
                             res <- getLine
                             putStr $ action res
                             totalREPL prompt action

-- *Exercise11-2> :total totalREPL
-- Main.totalREPL is Total

{-
*Exercise11-2> :exec run forever (totalREPL "\n: " toUpper)

: Hello
HELLO
: World
WORLD
-}