{-# LANGUAGE ParallelListComp, BangPatterns #-}

module ServerOperations (
    processGetRequest, 
    processHeadRequest, 
    processOptionsRequest, 
    processInvalidRequest,
    processRequest,
    ServerOperation)
where
    import SocketInteractions
    import RequestParser
    import OutputGenerators

    import Data.List

    import Network.Socket (send, Socket, sClose)


    type ServerOperation = Socket -> [String] -> IO()

    impementedRequests = [Head, Get, Options]


    -- runs a get request
    -- returns a tuple of the header and the file text 
    runGetRequest :: [String] -> OutputGenerator -> IO((String, String))
    runGetRequest args gen = do 
        let getArgs = getGetArguments $ getPath args
        let path = if getPath args == "/" then "./index.html" else getRelativePath args
        f <- gen path getArgs
        let out = concat ["Content-Length: ",
                show $ length f,
                "\r\n\r\n"]
        return ((out, f))

    -- processes a get request
    processGetRequest :: ServerOperation
    processGetRequest s args = do
        (out, f) <- runGetRequest args runHTML
        sendHttpOk s
        send s out
        send s f
        sClose s

    -- processes a head request
    processHeadRequest :: ServerOperation
    processHeadRequest s args = do
        (out, _) <- runGetRequest args runHTML
        sendHttpOk s
        send s out
        sClose s

    -- processes an options request
    processOptionsRequest :: ServerOperation
    processOptionsRequest s _ = do
        sendHttpOk s
        send s $ "Allow: " ++ 
            intercalate "," (map show impementedRequests)
        sClose s

    -- processes an invalid request
    processInvalidRequest :: ServerOperation
    processInvalidRequest s _ = sendHttpBadRequest s >> sClose s

    -- calls the relevant function for the request
    processRequest :: Socket -> IO()
    processRequest s = do
        args <- getArgs s
        let requestType = getRequest args
        helper requestType s args
        where 
            helper Head = processHeadRequest
            helper Get = processGetRequest
            helper Options = processOptionsRequest
            helper _ = processInvalidRequest