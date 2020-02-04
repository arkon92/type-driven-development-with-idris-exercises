module Main

import Data.Vect

infixr 5 .+.

data Schema = SChar
            | SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SChar = Char
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

data Command : Schema -> Type where
     SetSchema : (newSchema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Maybe Integer -> Command schema
     Quit : Command schema

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newItem = MkData schema _ (addToData store)
  where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SChar input = case span (/= ' ') (unpack input) of
                               ([], _) => Nothing
                               ( c :: Nil, rest) => Just (c, ltrim (pack rest))
                               _ => Nothing
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                            (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                            _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input = do (l_val, input') <- parsePrefix schemal input
                                             (r_val, input'') <- parsePrefix schemar input'
                                             pure ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("Char" :: xs) = case xs of
                                  [] => Just SChar
                                  _ => case parseSchema xs of
                                            Nothing => Nothing
                                            Just xs_sch => Just (SChar .+. xs_sch)
parseSchema ("String" :: xs) = case xs of
                                    [] => Just SString
                                    _ => do xs_sch <- parseSchema xs
                                            pure (SString .+. xs_sch)
parseSchema ("Int" :: xs) = case xs of
                                 [] => Just SInt
                                 _ => do xs_sch <- parseSchema xs
                                         pure (SInt .+. xs_sch)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "schema" rest = do schemaok <- parseSchema (words rest)
                                       pure (SetSchema schemaok)
parseCommand schema "add" rest = do restok <- parseBySchema schema rest
                                    pure (Add restok)
parseCommand schema "get" val = case (unpack val) of
                                     [] => Just (Get Nothing)
                                     _ => case all isDigit (unpack val) of
                                               False => Nothing
                                               True => Just (Get (Just (cast val)))
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SChar} item = show item
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (display (index id (items store)) ++ "\n", store)

zipWithIndex : Vect n elem -> Vect n (Nat, elem)
zipWithIndex v = withIndex Z v where
  withIndex : Nat -> Vect n elem -> Vect n (Nat, elem)
  withIndex _ [] = []
  withIndex idx (x :: xs) = (idx, x) :: withIndex (S idx) xs

displayAll : (store : DataStore) -> String
displayAll store = let store_items = (zipWithIndex (items store)) in
                       foldl (\acc, x => acc ++ (show (fst x)) ++ ": " ++ (display (snd x)) ++ "\n") ("") store_items

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S k => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
    = case parse (schema store) input of
           Nothing => Just ("Invalid command \n", store)
           Just (Add item) =>
             Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
           Just (SetSchema schema') => case setSchema store schema' of
                                            Nothing => Just ("Can't update schema\n", store)
                                            Just store' => Just ("OK\n", store')
           Just (Get pos) => case pos of
                                  Just position => getEntry position store
                                  Nothing => Just (displayAll store, store)
           Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput

{-
Command: schema Char Int
OK
Command: add x 24
ID 0
Command: add y 17
ID 1
Command: get 0
'x', 24
-}

{-
Command: schema Char Int
OK
Command: add x 24
ID 0
Command: add y 17
ID 1
Command: get
0: 'x', 24
1: 'y', 17
-}
