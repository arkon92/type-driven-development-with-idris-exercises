module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size store) newItem = MkData _ (addToData store)
  where
    addToData : Vect oldsize String -> Vect (S oldsize) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size                           
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store
    = let store_items = items store in
          case integerToFin pos (size store) of
               Nothing => Just ("Out of range\n", store)
               Just id => Just (index id (items store) ++ "\n", store)

zipWithIndex : Vect n elem -> Vect n (Nat, elem)
zipWithIndex v = withIndex Z v where
  withIndex : Nat -> Vect n elem -> Vect n (Nat, elem)
  withIndex _ [] = []
  withIndex idx (x :: xs) = (idx, x) :: withIndex (S idx) xs

searchItem : DataStore -> String -> (p ** Vect p (Nat, String))
searchItem store subStr = let withIndex = zipWithIndex (items store) in
                              Vect.filter (\s => Strings.isInfixOf subStr (snd s)) withIndex

formatFoundItems : (p ** Vect p (Nat, String)) -> String
formatFoundItems (_ ** []) = "No entry found in the store\n"
formatFoundItems ( _ ** x :: xs ) = let remainingsValues = case xs of
                                                           [] => ""
                                                           _ => formatFoundItems ( _ ** xs)
                                        currentIndex = show (fst x)
                                        currentValue = snd x in
                                        currentIndex ++ " : " ++ currentValue ++ "\n" ++ remainingsValues

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
    = case parse input of
           Nothing => Just ("Invalid command \n", store)
           Just (Add item) =>
             Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
           Just (Get pos) => getEntry pos store
           Just (Search str) => Just (formatFoundItems (searchItem store str), store)
           Just Size => Just (show (size store) ++ "\n", store)
           Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

{-
*Datastore> :exec
Command: add Shearer
ID 0
Command: add Milburn
ID 1
Command: add White
ID 2
Command: size
3
Command: search Mil
1 : Milburn
-}