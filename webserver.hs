{-# LANGUAGE ParallelListComp, BangPatterns #-}

import Network hiding (accept)
import Network.Socket
import Control.Concurrent
import ServerOperations hiding (getArgs)

import System.Environment (getArgs)
import Data.List
import Data.Maybe


defaultPort = 5002

findArgument :: [String] -> [String] -> Maybe Int
findArgument [] _ = Nothing
findArgument _ [] = Nothing
findArgument args (flag:flags) = 
    case elemIndex flag args of 
        Just x -> Just x
        Nothing -> findArgument args flags

portToUse :: [String] -> PortID
portToUse args = 
    case findArgument args ["-p", "--port"] of
        Just x -> PortNumber $ cleanNumber x
        Nothing -> PortNumber defaultPort 
    where
        cleanNumber :: Int -> PortNumber
        cleanNumber x
            | x + 1 < length args = fromIntegral (read $ args !! (x + 1) :: Int)
            | otherwise = defaultPort

 
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