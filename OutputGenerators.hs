{-# LANGUAGE ParallelListComp, BangPatterns #-}

module OutputGenerators (
    runPHP,
    runHTML,
    OutputGenerator)
where
    import System.Process
    import System.IO
    import Data.Maybe

    type OutputGenerator = String -> [(String, String)] -> IO (String)

    -- runs php on the given filename with the given GET arguments
    -- returns the output from php-cgi 
    runPHP :: OutputGenerator
    runPHP filename xs = do
        putStrLn $ show $ arguments
        
        (stdIn, stdOut, stdErr, p) <- 
            createProcess 
                (proc "php-cgi" arguments) 
                    {std_out = CreatePipe,
                     std_err = CreatePipe}
        
        out <- case stdOut of 
            Just x -> hGetContents x
            Nothing -> readFile filename

        e <- case stdErr of
            Just x -> do
                t <- hGetContents x
                putStrLn "Error: "
                putStrLn $ show $ t
            Nothing -> putStrLn ""
        return (out)
            where
                arguments = ["-f", filename] ++ [x ++ "=" ++ y | (x, y) <- xs]

    runHTML :: OutputGenerator
    runHTML filename xs = 
        readFile filename