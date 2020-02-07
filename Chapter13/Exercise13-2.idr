import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
     Push : Integer -> StackCmd () height (S height)
     Pop : StackCmd Integer (S height) height
     Top : StackCmd Integer (S height) (S height)

     GetStr : StackCmd String height height
     PutStr : String -> StackCmd () height height

     Pure : ty -> StackCmd ty height height
     (>>=) : StackCmd a height1 height2 ->
             (a -> StackCmd b height2 height3) ->
             StackCmd b height1 height3

data StackIO : Nat -> Type where
     Do : StackCmd a height1 height2 ->
          (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
     (>>=) : StackCmd a height1 height2 ->
             (a -> Inf (StackIO height2)) -> StackIO height1
     (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

runStack : (stk : Vect inHeight Integer) ->
  StackCmd ty inHeight outHeight -> IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (x', newStk) <- runStack stk x
                            runStack newStk (f x')

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f)
     = do (res, newStk) <- runStack stk c
          run fuel newStk (f res)
run Dry stk p = pure ()

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)

doSubtract : StackCmd () (S (S height)) (S height)
doSubtract = do val1 <- Pop
                val2 <- Pop
                Push (val2 - val1)

doMulltiply : StackCmd () (S (S height)) (S height)
doMulltiply = do val1 <- Pop
                 val2 <- Pop
                 Push (val1 * val2)

data StkInput = Number Integer
              | Add
              | Subtract
              | Multiply
              | Negate
              | Discard
              | Duplicate

strToInput : String -> Maybe StkInput
strToInput "add" = Just Add
strToInput "subtract" = Just Subtract
strToInput "multiply" = Just Multiply
strToInput "negate" = Just Negate
strToInput "discard" = Just Discard
strToInput "duplicate" = Just Duplicate
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

mutual
  tryAdd : StackIO height
  tryAdd {height = (S (S h))} = do doAdd
                                   result <- Top
                                   PutStr (show result ++ "\n")
                                   stackCalc
  tryAdd = do PutStr "Fewer than two items on the stack\n"
              stackCalc

  trySubstract : StackIO height
  trySubstract {height = (S (S h))} = do doSubtract
                                         result <- Top
                                         PutStr (show result ++ "\n")
                                         stackCalc
  trySubstract = do PutStr "Fewer than two items on the stack\n"
                    stackCalc

  tryMultiply : StackIO height
  tryMultiply {height = (S (S h))} = do doMulltiply
                                        result <- Top
                                        PutStr (show result ++ "\n")
                                        stackCalc
  tryMultiply = do PutStr "Fewer than two items on the stack\n"
                   stackCalc

  tryNegate : StackIO height
  tryNegate {height = (S h)} = do x <- Pop
                                  Push (-x)
                                  result <- Top
                                  PutStr (show result ++ "\n")
                                  stackCalc
  tryNegate = do PutStr "Nothing on the stack\n"
                 stackCalc

  tryDupplicate : StackIO height
  tryDupplicate {height = (S h)} = do x <- Top
                                      Push x
                                      PutStr ("Duplicated " ++ show x ++ "\n")
                                      stackCalc
  tryDupplicate = do PutStr "Nothing on the stack\n"
                     stackCalc

  tryDiscard : StackIO height
  tryDiscard {height = (S h)} = do x <- Pop
                                   PutStr ("Discarded " ++ show x ++ "\n")
                                   stackCalc
  tryDiscard = do PutStr "Nothing on the stack\n"
                  stackCalc

  stackCalc : StackIO height
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                      Nothing => do PutStr "Invalid input\n"
                                    stackCalc
                      Just (Number x) => do Push x
                                            stackCalc
                      Just Add => tryAdd
                      Just Subtract => trySubstract
                      Just Multiply => tryMultiply
                      Just Negate => tryNegate
                      Just Discard => tryDiscard
                      Just Duplicate => tryDupplicate

main : IO ()
main = run forever [] stackCalc

{-
*Exercise13-2> :exec
> 5
> 3
> subtract
2
> 8
> multiply
16
-}

{-
*Exercise13-2> :exec
> 10
> negate
-10
-}

{-
*Exercise13-2> :exec
> 3
> 4
> discard
Discarded 4
> add
Fewer than two items on the stack
-}

{-
*Exercise13-2> :exec
> 2
> duplicate
Duplicated 2
> add
4
-}