import Network hiding (accept)
import Network.Socket
import Control.Concurrent
import System.IO
import System.Process
import Data.Maybe
import Data.List
import Data.List.Split

-- Request data type
-- Used for pattern matching
data Request = Invalid | Option | Delete | Head | Get | Post | Put deriving (Show, Eq)
 
main = withSocketsDo $ do
    putStrLn "listening"
    socket <- listenOn $ PortNumber 5002
    mainLoop socket

-- main loop
-- forks on new connect
mainLoop :: Socket -> IO()
mainLoop socket = do
    (h,_) <- accept socket
    forkIO $ processRequest h
    mainLoop socket

-- gets the arguments from the socket as a list of strings
-- removes any " symbols
-- TODO: Change so only those at the front and start are removed
getArgs :: Socket -> IO ([String])
getArgs s = do  
    b <- recv s 1024
    let t = filter (/='"') (show b)
    return (words $ t)

-- returns the path from a request
-- assumes xs[1] === path
getPath :: [String] -> String
getPath xs = xs !! 1 

-- returns the relative path from a request
-- appends a . to the start to allow for file loading
-- removes get arguments
getRelativePath :: [String] -> String
getRelativePath = (\x -> '.' : splitted x) . getPath
    where 
        splitted = head . splitOn "?"

-- returns the request type
-- any unknown get mapped to invalid
-- TODO: add support for option/connect/trace
getRequest :: [String] -> Request
getRequest xs = case head xs of
    "DELETE" -> Delete
    "GET" -> Get
    "HEAD" -> Head
    "POST" -> Post
    "PUT" -> Put
    _ -> Invalid

-- returns get arguments from the url as tuples of length 2
-- where !! 0 == key, !! 1 == value
-- TODO: refactor
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

-- runs php on the given filename with the given GET arguments
-- returns the output from php-cgi 
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

-- runs a get request
-- returns a tuple of the header and the file text 
runGetRequest :: [String] -> IO((String, String))
runGetRequest args = do 
    let getArgs = getGetArguments $ getPath args
    let path = if getPath args == "/" then "./index.html" else getRelativePath args
    f <- runPHP path getArgs
    let out = concat ["HTTP/1.0 200 OK\r\nContent-Length: ",
            show $ length f,
            "\r\n\r\n"]
    return ((out, f))

-- processes a get request
processGetRequest :: Socket -> [String] -> IO()
processGetRequest s args = do
    (out, f) <- runGetRequest args
    send s out
    send s f
    sClose s

-- processes a head request
processHeadRequest :: Socket -> [String] -> IO()
processHeadRequest s args = do
    (out, _) <- runGetRequest args
    send s out
    sClose s

-- processes an option request
processOptionRequest :: Socket -> [String] -> IO()
processOptionRequest s _ = do
    send s "HTTP/1.0 200 OK"
    send s $ "Allow: " ++ 
        intercalate "," (map show [Head, Get, Option])
    send s "\r\n\r\n"
    sClose s

-- processes an invalid request
processInvalidRequest :: Socket -> [String] -> IO()
processInvalidRequest s _ = do
    send s "HTTP/1.0 400 Bad Request"
    send s "\r\n\r\n"
    sClose s


-- calls the relevant function for the request
processRequest :: Socket -> IO()
processRequest s = do
    args <- getArgs s
    let requestType = getRequest args
    helper requestType s args
    where 
        helper Head = processHeadRequest
        helper Get = processGetRequest
        helper Option = processOptionRequest
        helper _ = processInvalidRequest