import Data.Primitives.Views
import System

%default total

data Input = Answer Int
           | QuitCmd

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b
     ReadFile : String -> Command (Either FileError String)
     WriteFile : (filePath : String) -> (content : String) -> Command (Either FileError ())

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf $ ConsoleIO b) -> ConsoleIO b
  (>>=) = Do

data Fuel = Empty | More (Lazy Fuel)

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand (f res)
runCommand (ReadFile f) = readFile f
runCommand (WriteFile f c) = writeFile f c

partial
forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Empty y = pure Nothing
run (More x) (Quit y) = pure $ Just y
run (More x) (Do z f) = do res <- runCommand z
                           run x $ f res

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure $ Answer (cast answer)

mutual
  correct : Stream Int -> (score : Nat) -> (trialsNumber : Nat) -> ConsoleIO Nat
  correct nums score trialsNumber = do PutStr "Correct!\n"
                                       quiz nums (score + 1) (trialsNumber + 1)

  wrong : Stream Int -> Int -> (score : Nat) -> (trialsNumber : Nat) -> ConsoleIO Nat
  wrong nums ans score trialsNumber = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                                         quiz nums score  (trialsNumber + 1)

  quiz : Stream Int -> (score : Nat) -> (trialsNumber : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score trialsNumber = do PutStr ("Score so far: " ++ show score
                                                        ++ " | Attempts: " ++ show trialsNumber ++ "\n")
                                                      input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
                                                      case input of
                                                           (Answer x) => if x == (num1 * num2)
                                                                            then correct nums score trialsNumber
                                                                            else wrong nums (num1 * num2) score trialsNumber
                                                           QuitCmd => Quit score

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where bound : Int -> Int
        bound x with (divides x 12)
          bound ((12 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
              | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)


{-
*Exercise11-3> :exec
Score so far: 0 | Attempts: 0
12 * 4? 48
Correct!
Score so far: 1 | Attempts: 1
9 * 10? 90
Correct!
Score so far: 2 | Attempts: 2
1 * 7? 0
Wrong, the answer is 7
Score so far: 2 | Attempts: 3
6 * 5? 30
Correct!
Score so far: 3 | Attempts: 4
8 * 2? quit
Final score: 3
-}



data Shell : Type where
  Cat : String -> Shell
  Copy : (source : String) -> (destination : String) -> Shell
  Error : Shell
  Exit : Shell

readShellInput : (prompt : String) -> Command Shell
readShellInput prompt = do PutStr prompt
                           answer <- GetLine
                           case words answer of
                                ["cat", f] => Pure $ Cat f
                                ["copy", src, dest] => Pure $ Copy src dest
                                ["exit"] => Pure Exit
                                _ => Pure Error

shell : ConsoleIO String
shell = do input <- readShellInput "\n#: "
           case input of
                (Cat x) => do content <- ReadFile x
                              case content of
                                   (Left l) => do PutStr "Can't read file\n"
                                                  shell
                                   (Right r) => do PutStr r
                                                   shell
                (Copy source destination) => do content <- ReadFile source
                                                case content of
                                                     (Left l) => do PutStr "Can't read file content\n"
                                                                    shell
                                                     (Right r) => do WriteFile destination r
                                                                     shell
                Error => do PutStr "Can't process command"
                            shell
                Exit => Quit "Exit"

{-
*Exercise11-3> :exec run forever shell

#: c
Can't process command
#: copy Exercise11-3.idr test.idr

#: exit
Just "Done"
-}