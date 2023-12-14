module Main (main) where
import System.Environment (getArgs)
import Lib (readExpr, eval, trapError, extractValue, LispVal)
import System.IO (hFlush, stdout, hPutStrLn, stderr)

readLine :: String -> IO String
readLine str = putStr str >> hFlush stdout >> getLine

evalReadedLine :: [(String, LispVal)] -> IO ()
evalReadedLine env = do
    line <- readLine ">> "
    case line of
        "quit" -> return ()
        _ -> do putStrLn $ evalArgs line
                evalReadedLine env

evalArgs :: String -> String
evalArgs arg = extractValue $ trapError evaled
    where
        evaled = fmap show $ readExpr arg >>= eval

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> evalReadedLine []
        1 -> putStrLn $ evalArgs $ head args
        _ -> hPutStrLn stderr "USAGE: 1 or 0 arguments are required"