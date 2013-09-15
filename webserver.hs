import Network hiding (accept)
import Network.Socket
import Control.Concurrent
import System.IO
import System.Process
import Data.Maybe
import Data.List.Split


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
getRelativePath = (\x -> '.' : splitted x) . getPath
    where 
        splitted = head . splitOn "?"

getRequest :: [String] -> Request
getRequest xs = case head xs of
    "DELETE" -> Delete
    "GET" -> Get
    "HEAD" -> Head
    "POST" -> Post
    "PUT" -> Put
    _ -> Invalid

getGetArguments :: String -> [(String, String)]
getGetArguments xs = if elem '?' xs then 
                        [(x, y) | d <- ys, 
                         let (x, y) = helper $ splitOn "=" d, 
                         x /= ""]
                     else []
    where 
        helper :: [String] -> (String, String)
        helper [] = ("","")
        helper [x] = (x, "")
        helper (x:xs) = (x, concat $ xs)
        ys = splitOn "&" $ tail $ dropWhile (/= '?') xs 

runPHP :: String -> [(String, String)] -> IO (String)
runPHP filename xs = do
    putStrLn $ show $ arguments
    (stdIn, stdOut, stdErr, p) <- 
        createProcess 
            (proc "php-cgi" arguments) 
                {std_out = CreatePipe}
    out <- case stdOut of 
        Just x -> hGetContents x
        Nothing -> readFile filename
    return (out)
        where
            arguments = ["-f", filename] ++ [x ++ "=" ++ y | (x, y) <- xs]

runGetRequest :: [String] -> IO((String, String))
runGetRequest args = do 
    let getArgs = getGetArguments $ getPath args
    let path = if getPath args == "/" then "./index.html" else getRelativePath args
    f <- runPHP path getArgs
    let out = concat ["HTTP/1.0 200 OK\r\nContent-Length: ",
            show $ length f,
            "\r\n\r\n"]
    return ((out, f))

processGetRequest :: Socket -> [String] -> IO()
processGetRequest s args = do
    (out, f) <- runGetRequest args
    send s out
    send s f
    sClose s

processHeadRequest :: Socket -> [String] -> IO()
processHeadRequest s args = do
    (out, _) <- runGetRequest args
    send s out
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
        helper Head = processHeadRequest
        helper Get = processGetRequest
        helper _ = processInvalidRequest