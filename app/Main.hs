module Main (main) where
import System.Environment (getArgs)
import Lib (readExpr, eval, extractValue, trapError, Env)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Parser (LispVal)

readLine :: String -> IO String
readLine str = putStr str >> hFlush stdout >> getLine

evalReadedLine :: [(String, LispVal)] -> IO ()
evalReadedLine env = do
    line <- readLine ">> "
    case line of
        "quit" -> return ()
        _ -> do
                let (newStr, newEnv) = evalArgs env line
                putStrLn newStr
                evalReadedLine newEnv

evalArgs :: Env -> String -> (String, Env)
evalArgs env arg = newEval evaled
    where
        evaled = readExpr arg >>= eval env
        newEval (Left err) = (show err, env)
        newEval (Right (newVal, newEnv)) = (show newVal, newEnv)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> evalReadedLine []
        1 -> putStrLn $ fst $ evalArgs [] $ head args
        _ -> hPutStrLn stderr "USAGE: 1 or 0 arguments are required"
