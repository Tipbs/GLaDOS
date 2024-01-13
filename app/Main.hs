module Main (main) where
import System.Environment (getArgs)
import Lib (readExpr, eval, extractValue, trapError, Env)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Parser (LispVal)
import Wasm (buildWasm)
import Data.Binary (Word8)
import KopeParser (parseFile)
import KopeParserLib (KopeVal (KopeArray))
import qualified Data.ByteString as BS

type Compile = (String, String) -- input output
type Exec = String
type Output = String
data Args = Args (Maybe Compile) (Maybe Exec)

parseArgs :: [String] -> Maybe Args
parseArgs ["-c", str, "-o", out] = Just $ Args (Just (str, out)) Nothing
parseArgs ["-e", str] = Just $ Args Nothing (Just str)
parseArgs _ = Nothing

buildFile :: String -> IO (Either String [Word8])
buildFile path = do
    parsed <- parseFile path
    case parsed of
        Nothing -> return $ Left "Error while parsing"
        (Just (KopeArray arr)) -> return $ buildWasm arr
        _ -> return $ Left "Impossible case"

printBuilded :: String -> String -> IO ()
printBuilded input output = do
    builded <- buildFile input
    case builded of
        (Right val) -> BS.writeFile output (BS.pack val)
        (Left err) -> putStrLn err

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Args (Just (input, output)) Nothing) -> printBuilded input output
        Just (Args Nothing (Just exec)) -> putStrLn ("compiled: " ++ exec)
        _ -> hPutStrLn stderr "USAGE: ./glados [-c file.kop -o output.wasm] | [-e file.wasm]"
