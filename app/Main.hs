module Main (main) where
import System.Environment (getArgs)
import Wasm (buildWasm)
import Data.Binary (Word8)
import KopeParser (parseFile)
import KopeParserLib (KopeVal (KopeArray))
import qualified Data.ByteString as BS
import WASMParser (wasmParser, WasmModule (WasmModule), WasmFunction (..))
import VirtualM (exec)
import System.IO (hPutStrLn, stderr)

type Compile = (String, String) -- input output
type Exec = String
data Args = Args (Maybe Compile) (Maybe Exec)

parseArgs :: [String] -> Maybe Args
parseArgs ["-c", str, "-o", out] = Just $ Args (Just (str, out)) Nothing
parseArgs ["-r", str] = Just $ Args Nothing (Just str)
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

findMain :: WasmModule -> Maybe Int
findMain (WasmModule funcs _) = findFunc funcs 0
    where
        findFunc :: [WasmFunction] -> Int -> Maybe Int
        findFunc ((WasmFunction _ n): _) i | n == "main" = Just i
        findFunc ((WasmFunction _ _): xs) i = findFunc xs (i + 1)
        findFunc [] _ = Nothing

executeMain :: WasmModule -> IO (Either String Int)
executeMain modu@(WasmModule _ bodies) = case main_index of
    Just i -> let (mainB, local_decl_count) = bodies !! i
                in exec mainB [] (replicate local_decl_count 0) modu
    Nothing -> return $ Left "Couldn't find main in the exports"
    where
        main_index = findMain modu

printCompiled :: String -> IO ()
printCompiled path = do
    parsed <- wasmParser path
    case parsed of
        Right modu -> do
            -- putStrLn $ "Module in VM: " ++ show modu
            executeM <- executeMain modu
            case executeM of
                Right val -> putStrLn $ "The final value is " ++ show val
                Left err -> hPutStrLn stderr ("Error while executing the bytecode: " ++ err)

        Left err -> hPutStrLn stderr err
    
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Args (Just (input, output)) Nothing) -> printBuilded input output
        Just (Args Nothing (Just path)) -> printCompiled path
        _ -> hPutStrLn stderr "USAGE: ./glados [-c file.kop -o output.wasm] | [-r file.wasm]"
