module Time

import System.Clock

export
getTime : IO (Clock UTC)
getTime = clockTime UTC

export
getTimeMono : IO (Clock Monotonic)
getTimeMono = clockTime Monotonic

main : IO ()
main = do t <- getTime
          putStr ("The current time is: " ++ (show (seconds t)) ++ ".\n")
          m <- getTimeMono
          putStr ("The monotonic time is: " ++ (show (nanoseconds m)) ++ "ns .\n")
