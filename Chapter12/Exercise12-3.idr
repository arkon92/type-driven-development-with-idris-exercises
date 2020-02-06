import Data.Primitives.Views
import System

%default total

record Score where
       constructor MkScore
       correct : Nat
       attempted : Nat

record GameState where
       constructor MkGameState
       score : Score
       difficulty : Int

Show GameState where
    show st = show (correct (score st)) ++ "/"
              ++ show (attempted (score st)) ++ "\n"
              ++ "Difficulty: " ++ show (difficulty st)

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String

     GetRandom : Command Int
     GetGameState : Command GameState
     PutGameState : GameState -> Command ()

     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

mutual
  Functor Command where
      map func x = do x' <- x
                      pure (func x')

  Applicative Command where
      pure = Pure
      (<*>) f a = do f' <- f
                     a' <- a
                     pure (f' a')

  Monad Command where
      (>>=) = Bind

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'


runCommand : Stream Int -> GameState -> Command a -> IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do putStr x
                                      pure ((), rnds, state)
runCommand rnds state GetLine = do str <- getLine
                                   pure (str, rnds, state)

runCommand (val :: rnds) state GetRandom = pure (getRandom val (difficulty state), rnds, state)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
      getRandom val 0 | DivByZero = 1
      getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1
runCommand rnds state GetGameState
      = pure (state, rnds, state)
runCommand rnds state (PutGameState newState)
      = pure ((), rnds, newState)

runCommand rnds state (Pure val)
      = pure (val, rnds, state)
runCommand rnds state (Bind c f)
      = do (res, newRnds, newState) <- runCommand rnds state c
           runCommand newRnds newState (f res)

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Stream Int -> GameState -> ConsoleIO a -> IO (Maybe a, Stream Int, GameState)
run fuel rnds state (Quit val) = do pure (Just val, rnds, state)
run (More fuel) rnds state (Do c f)
     = do (res, newRnds, newState) <- runCommand rnds state c
          run fuel newRnds newState (f res)
run Dry rnds state p = pure (Nothing, rnds, state)

updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do st <- GetGameState
                       PutGameState (f st)

addWrong : GameState -> GameState
addWrong = record { score->attempted $= (+1) }

addCorrect : GameState -> GameState
addCorrect = record { score->correct $= (+1),
                      score->attempted $= (+1) }

mutual
  correct : ConsoleIO GameState
  correct = do PutStr "Correct!\n"
               updateGameState addCorrect
               quiz

  wrong : Int -> ConsoleIO GameState
  wrong ans
        = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
             updateGameState addWrong
             quiz

  data Input = Answer Int
             | QuitCmd

  readInput : (prompt : String) -> Command Input
  readInput prompt
     = do PutStr prompt
          answer <- GetLine
          if toLower answer == "quit"
             then Pure QuitCmd
             else Pure (Answer (cast answer))

  quiz : ConsoleIO GameState
  quiz = do num1 <- GetRandom
            num2 <- GetRandom
            st <- GetGameState
            PutStr (show st ++ "\n")

            input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
            case input of
               Answer answer => if answer == num1 * num2
                                   then correct
                                   else wrong (num1 * num2)
               QuitCmd => Quit st

initState : GameState
initState = MkGameState (MkScore 0 0) 12

partial
main : IO ()
main = do seed <- time
          (Just score, _, state) <-
              run forever (randoms (fromInteger seed)) initState quiz
                  | _ => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show state)

{-
*Exercise12-3> :exec
0/0
Difficulty: 12
3 * 4? 12
Correct!
1/1
Difficulty: 12
3 * 1? 3
Correct!
2/2
Difficulty: 12
6 * 7? quit
Final score: 2/2
Difficulty: 12
-}


record Votes where
  constructor MkVotes
  upvotes : Integer
  downvotes : Integer

record Article where
  constructor MkArticle
  title : String
  url : String
  score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

getScore : Article -> Integer
getScore article = upvotes (score article) - downvotes (score article)

{-
*Exercise12-3> getScore goodSite
94 : Integer
*Exercise12-3> getScore badSite
-42 : Integer
-}


addUpvote : Article -> Article
addUpvote = record { score->upvotes $= (+1) }

addDownvote : Article -> Article
addDownvote = record { score->downvotes $= (+1) }

{-
*Exercise12-3> addUpvote goodSite
MkArticle "Good Page" "http://example.com/good" (MkVotes 102 7) : Article
*Exercise12-3> addDownvote  badSite
MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 48) : Article
*Exercise12-3> getScore (addUpvote goodSite)
95 : Integer
-}