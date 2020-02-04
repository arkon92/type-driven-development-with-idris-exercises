module Main

import Question3
import Question6

main : IO ()
main = repl "\n>Enter a String: " (show . palindrome)
-- main = repl "\n>Enter a String: " (show . counts)
