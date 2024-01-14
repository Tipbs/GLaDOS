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
import WASMParser (wasmParser, WasmModule (WasmModule))
import VirtualM (exec)

type Compile = (String, String) -- input output
type Exec = String
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
        (Left err) -> hPutStrLn stderr err

printCompiled :: String -> IO ()
printCompiled path = do
    parsed <- wasmParser path
    case parsed of
        Right modu@(WasmModule funcs bodies) -> do
            putStrLn $ "Module in VM: " ++ show modu
            let executed = exec (fst $ head bodies) [] [4, 5] modu
            case executed of
                Right val -> putStrLn $ "The final value is " ++ show val
                Left err -> hPutStrLn stderr ("Error while executing the bytecode: " ++ err)

        Left err -> hPutStrLn stderr err
    
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Args (Just (input, output)) Nothing) -> printBuilded input output
        Just (Args Nothing (Just exec)) -> printCompiled exec
        _ -> hPutStrLn stderr "USAGE: ./glados [-c file.kop -o output.wasm] | [-e file.wasm]"
