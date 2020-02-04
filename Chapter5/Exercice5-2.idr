import System

parseNumber : String -> Maybe Nat
parseNumber "" = Nothing
parseNumber s = if all isDigit (unpack s)
                   then Just (cast s)
                   else Nothing

mutual
    handleCorectlyFormattedGuess : (formattedGuess : Nat) -> (target: Nat) -> (guesses : Nat) -> IO ()
    handleCorectlyFormattedGuess g target guesses@(S a) = if g < 101 && g >0
                                                     then case compare g target of
                                                          LT => do putStrLn "Your guess is too low"
                                                                   guess target guesses
                                                          GT => do putStrLn "Your guess is too high"
                                                                   guess target guesses
                                                          EQ => putStrLn ("Your guess is correct. You found it in " ++ (show a) ++ " attempts.")
                                                     else do putStrLn "Invalid guess. Your guess should be in the range [1, 100]"
                                                             guess target guesses

    guess : (target : Nat) -> (guesses : Nat) -> IO ()
    guess target guesses = do putStr ("Attempt number " ++ (show guesses) ++" Type your guess: ")
                              userInput <- getLine
                              userGuess <- pure (parseNumber userInput)
                              let incrementedCounter = S guesses
                              case userGuess of
                                Just g => handleCorectlyFormattedGuess g target incrementedCounter
                                Nothing => do putStrLn "Invalid guess format. You shoud enter a number"
                                              guess target incrementedCounter

main : IO ()
main = do randomValue <- time
          let target = (mod randomValue 100) + 1
          guess (cast target) (S Z)


myRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myRepl prompt onInput = do  putStr prompt
                            input <- getLine
                            putStr (onInput input)
                            myRepl prompt onInput

myReplWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt onInput = do putStr prompt
                                     input <- getLine
                                     case onInput state input of
                                       Just (res, newState) => do putStr res
                                                                  myReplWith newState prompt onInput
                                       Nothing => putStr ""