module Main

import Average
import Time
import Control.Linear.LIO
import Control.Linear.Network
import System
import System.Clock

prepMsg : String -> IO (String)
prepMsg msg = do st <- getTimeMono
                 let output = msg ++ "\n" ++ showAverage msg
                 sleep <- sleep 1
                 et <- getTimeMono
                 let delta = timeDifference et st
                 pure $ (output ++ "\n took " ++ show delta)

echo : LinearIO io => (1 _ : Socket Open) -> L io ()
echo conn
    = do Just msg # conn <- recv conn 1024
         | Nothing # conn => done conn
         resp <- liftIO (prepMsg (fst msg))
         True # conn <- send conn ("Echo: " ++ resp)
         | False # conn => done conn
         conn <- close conn
         done conn

echoLoop : LinearIO io => (1 _ : Socket Listening) -> L io ()
echoLoop sock
    = do True # (sock, conn) <- accept sock
         | False # sock => echoLoop sock
         putStrLn "Connection received"
         echo conn
         echoLoop sock

echoServer : LinearIO io => L io ()
echoServer
    = newSocket AF_INET Stream 0
         (\sock => 
              do True # sock <- bind sock Nothing 9443
                 | False # sock => do putStrLn "Bind failed"
                                      done sock
                 True # sock <- listen sock
                 | False # sock => do putStrLn "Listen failed"
                                      done sock
                 putStrLn "Server running"
                 echoLoop sock)
         (\err => putStrLn $ "Failed: " ++ show err)

runEcho : IO ()
runEcho = run echoServer
