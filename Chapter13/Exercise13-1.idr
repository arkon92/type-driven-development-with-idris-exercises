namespace DoorState

  data DoorState = DoorOpen | DoorClosed

  data DoorCmd : Type -> DoorState -> DoorState -> Type where
      Open : DoorCmd () DoorClosed DoorOpen
      Close : DoorCmd () DoorOpen DoorClosed
      RingBell : DoorCmd () state state

      Pure : ty -> DoorCmd ty state state
      (>>=) : DoorCmd a state1 state2 ->
              (a -> DoorCmd b state2 state3) ->
              DoorCmd b state1 state3

  doorProg : DoorCmd () DoorClosed DoorClosed
  doorProg = do RingBell
                Open
                RingBell
                Close


namespace GuessCmd
  data GuessCmd : Type -> Nat -> Nat -> Type where
      Try : Integer -> GuessCmd Ordering (S guesses) guesses

      Pure : ty -> GuessCmd ty state state
      (>>=) : GuessCmd a state1 state2 ->
              (a -> GuessCmd b state2 state3) ->
              GuessCmd b state1 state3

  threeGuesses : GuessCmd () 3 0
  threeGuesses = do Try 10
                    Try 20
                    Try 15
                    Pure ()

{-
noGuess : GuessCmd () 0 0
noGuess = do Try 10
             Pure ()

   |
35 | noGuess = do Try 10
   |              ~~~~~~
When checking right hand side of noGuess with expected type
        GuessCmd () 0 0

When checking an application of constructor Main.GuessCmd.>>=:
        Type mismatch between
                GuessCmd Ordering (S guesses) guesses (Type of Try _)
        and
                GuessCmd a 0 state2 (Expected type)

        Specifically:
                Type mismatch between
                        S guesses
                and
                        0
-}


namespace Matter

  data Matter = Solid | Liquid | Gas

  data MatterCmd : Type -> Matter -> Matter -> Type where
       Melt : MatterCmd () Solid Liquid
       Boil : MatterCmd () Liquid Gas
       Condense : MatterCmd () Gas Liquid
       Freeze : MatterCmd () Liquid Solid

       Pure : ty -> MatterCmd ty state state
       (>>=) : MatterCmd a state1 state2 ->
               (a -> MatterCmd b state2 state3) ->
               MatterCmd b state1 state3

  iceSteam : MatterCmd () Solid Gas
  iceSteam = do Melt
                Boil

  steamIce : MatterCmd () Gas Solid
  steamIce = do Condense
                Freeze

{-
  overMelt : MatterCmd () Solid Gas
  overMelt = do Melt
                Melt

   |
85 |   overMelt = do Melt
   |                 ~~~~
When checking right hand side of overMelt with expected type
        MatterCmd () Solid Gas

When checking an application of constructor Main.Matter.>>=:
        Type mismatch between
                MatterCmd () Solid Liquid (Type of Melt)
        and
                MatterCmd () Liquid Gas (Expected type)

        Specifically:
                Type mismatch between
                        Solid
                and
                        Liquid
-}
