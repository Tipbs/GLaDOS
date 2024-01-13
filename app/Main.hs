module Main (main) where
import System.Environment (getArgs)
import Lib (readExpr, eval, extractValue, trapError, Env)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Parser (LispVal)
import Wasm (buildWasm)
import Data.Binary (Word8)
import KopeParser (parseFile)
import KopeParserLib (KopeVal (KopeArray))

type Compile = String
type Exec = String
data Args = Args (Maybe Compile) (Maybe Exec)

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

parseArgs :: [String] -> Maybe Args
parseArgs ["-c", str] = Just $ Args (Just str) Nothing
parseArgs ["-e", str] = Just $ Args Nothing (Just str)
parseArgs _ = Nothing

buildFile :: String -> IO (Either String [Word8])
buildFile path = do
    parsed <- parseFile path
    case parsed of
        Nothing -> return $ Left "Error while parsing"
        (Just (KopeArray arr)) -> return $ buildWasm arr
        _ -> return $ Left "Impossible case"

printBuilded :: String -> IO ()
printBuilded path = do
    builded <- buildFile path
    case builded of
        (Right val) -> print val
        (Left err) -> putStrLn err

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Args (Just compile) Nothing) -> printBuilded compile
        Just (Args Nothing (Just exec)) -> putStrLn ("compiled: " ++ exec)
        _ -> hPutStrLn stderr "USAGE: ./glados [-c file.kop] | [-e file.wasm]"
