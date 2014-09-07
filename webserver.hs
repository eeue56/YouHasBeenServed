{-# LANGUAGE ParallelListComp, BangPatterns #-}

import Network hiding (accept)
import Network.Socket
import Control.Concurrent
import ServerOperations


portToUse = 5002
 
main = withSocketsDo $ do
    putStrLn "listening"
    socket <- listenOn $ PortNumber portToUse
    mainLoop socket

-- main loop
-- forks on new connect
mainLoop :: Socket -> IO()
mainLoop socket = do
    (h,_) <- accept socket
    forkIO $ processRequest h
    mainLoop socket