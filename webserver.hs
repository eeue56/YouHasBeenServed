import Network hiding (accept)
import Network.Socket
import Control.Concurrent
import System.IO

data Request = Invalid | Delete | Head | Get | Post | Put deriving (Show, Eq)
 
main = withSocketsDo $ do
    putStrLn "listening"
    socket <- listenOn $ PortNumber 5002
    mainLoop socket

mainLoop :: Socket -> IO()
mainLoop socket = do
    (h,_) <- accept socket
    forkIO $ processRequest h
    mainLoop socket

getArgs :: Socket -> IO ([String])
getArgs s = do  
    b <- recv s 1024
    let t = filter (/='"') (show b)
    return (words $ t)

getPath :: [String] -> String
getPath xs = xs !! 1

getRelativePath :: [String] -> String
getRelativePath xs = '.' : (xs !! 1)

getRequest :: [String] -> Request
getRequest xs = case head xs of
    "DELETE" -> Delete
    "GET" -> Get
    "HEAD" -> Head
    "POST" -> Post
    "PUT" -> Put
    _ -> Invalid

processGetRequest :: Socket -> [String] -> IO()
processGetRequest s args = do
    let path = if getPath args == "/" then "./index.html" else getRelativePath args
    f <- readFile path
    let sender = send s
    sender "HTTP/1.0 200 OK\r\nContent-Length: "
    sender $ show $ length f
    sender "\r\n\r\n"
    sender f
    sClose s

processInvalidRequest :: Socket -> [String] -> IO()
processInvalidRequest s _ = do
    send s "HTTP/1.0 400 Bad Request"
    send s "\r\n\r\n"
    sClose s

processRequest :: Socket -> IO()
processRequest s = do
    args <- getArgs s
    let requestType = getRequest args
    helper requestType s args
    where 
        helper Get = processGetRequest
        helper _ = processInvalidRequest