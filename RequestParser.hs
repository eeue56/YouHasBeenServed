{-# LANGUAGE ParallelListComp, BangPatterns #-}

module RequestParser (
    getPath,
    getRelativePath,
    getRequest,
    getGetArguments,
    Request(..))
where
    import Data.List.Split

    -- Request data type
    -- Used for pattern matching
    data Request = Invalid | 
        Options | 
        Delete | 
        Head | 
        Get | 
        Post | 
        Put deriving (Show, Eq, Ord)


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
        "OPTIONS" -> Options
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