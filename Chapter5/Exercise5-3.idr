import Data.Vect

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "")
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)


formatReading : List String -> String
formatReading [] = ""
formatReading (x :: []) = x
formatReading (x :: xs) = x ++ "\n" ++ (formatReading xs)

readAndSave : IO ()
readAndSave = do lines <- readToBlank
                 putStrLn "Enter a file path: "
                 filePath <- getLine
                 Right r <- writeFile filePath (formatReading lines)
                   | Left err => putStrLn (show err)
                 putStrLn ("File saved to: " ++ filePath)


readVectFromFile : File -> IO (len ** Vect len String)
readVectFromFile h = do Right x <- fGetLine h
                          | Left err => pure (_ ** [])
                        endOfFile <- fEOF h
                        if endOfFile
                          then pure (_ ** [])
                          else do (_ ** xs) <- readVectFromFile h
                                  pure (_ ** (trim x) :: xs)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right h <- openFile filename Read
                             | Left err => do pure (_ ** [])
                           result <- readVectFromFile h
                           closeFile h
                           pure result