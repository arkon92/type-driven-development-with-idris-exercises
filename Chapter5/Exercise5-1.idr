printLogger : IO ()
printLogger = do putStr "First string: "
                 fstString <- getLine
                 putStr "Second string: "
                 sndString <- getLine
                 let fstLength = length fstString
                 let sndLength = length sndString
                 putStrLn (show (maximum fstLength sndLength))

{-
*Exercise5-1> :exec printLogger
First string: short
Second string: longer
6
-}


printLoggerWithoutDo : IO ()
printLoggerWithoutDo = putStr "First string: " >>= \_ =>
                       getLine >>= \fstString =>
                       putStr "Second string: " >>= \_ =>
                       getLine >>= \sndString =>
                       let fstLength = length fstString in
                       let sndLength = length sndString in
                       putStrLn (show (maximum fstLength sndLength))

{-
*Exercise5-1> :exec printLoggerWithoutDo
First string: short
Second string: longer
6
-}