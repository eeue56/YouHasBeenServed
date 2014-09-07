{-# LANGUAGE ParallelListComp, BangPatterns #-}

import Network hiding (accept)
import Network.Socket
import Control.Concurrent
import ServerOperations hiding (getArgs)

import System.Environment (getArgs)
import Data.List
import Data.Maybe


defaultPort = 5002

portToUse :: [String] -> PortID
portToUse args = 
    case elemIndex "-p" args of
        Just x -> PortNumber $ fromIntegral (read (args !! (x + 1)) :: Int)
        Nothing -> PortNumber defaultPort 

 
main = withSocketsDo $ do
    args <- getArgs
    let port = portToUse args
    socket <- listenOn $ portToUse args
    putStrLn $ concat ["listening on ", show port] 
    mainLoop socket

-- main loop
-- forks on new connect
mainLoop :: Socket -> IO()
mainLoop socket = do
    (h,_) <- accept socket
    forkIO $ processRequest h
    mainLoop socket