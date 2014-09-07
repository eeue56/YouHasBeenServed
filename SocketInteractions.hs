{-# LANGUAGE ParallelListComp, BangPatterns #-}

module SocketInteractions (getArgs, 
    sendHttpVersionHeader,
    sendHttpHeader,
    sendHttpOk,
    sendHttpBadRequest)
where
    import Network.Socket 


    sendHeader :: String -> Socket -> IO()
    sendHeader header socket = send socket (header ++ "\r\n") >> return () 

    sendHttpVersionHeader :: String -> String -> Socket -> IO()
    sendHttpVersionHeader version message socket = sendHeader (version ++ message) socket 

    sendHttpHeader = sendHttpVersionHeader "HTTP/1.0"
    
    sendHttpOk = sendHttpHeader "200 OK"
    sendHttpBadRequest = sendHttpHeader "400 Bad Request"
    

    -- gets the arguments from the socket as a list of strings
    -- removes any " symbols
    -- TODO: Change so only those at the front and start are removed
    getArgs :: Socket -> IO ([String])
    getArgs s = do  
        b <- recv s 1024
        let t = filter (/='"') (show b)
        return (words $ t)