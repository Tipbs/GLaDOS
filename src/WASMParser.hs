module WASMParser () where
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word

data WasmModule = WasmModule {
    wasmFuncs :: [WasmFunction]
}

data WasmFunction = WasmFunction {
    functionIndex :: Integer
    -- instructions :: ???
}

instance Show WasmModule where
    show (WasmModule functions) = "Functions: " ++ show functions

instance Show WasmFunction where
    show (WasmFunction functionIndex) = show functionIndex

parseWasmModule :: Get WasmModule
parseWasmModule = do
    magic <- getWord32le
    if magic /= 0x6d736100
        then
            fail "Wrong magic number"
        else do
            -- implémenter parsing pour les sections, types, functions, etc

            return $ WasmModule {
                wasmFuncs = []
            }

parseWasmFile :: FilePath -> IO (Either String WasmModule)
parseWasmFile filePath = do
    fileContent <- BL.readFile filePath

    case runGetOrFail parseWasmModule fileContent of
        Left (_, _, errMsg) -> return $ Left errMsg
        Right (_, _, wasmModule) -> return $ Right wasmModule

main :: IO ()
main = do
    let wasmFilePath = "simple.wasm"
    parsedResult <- parseWasmFile wasmFilePath

    case parsedResult of
        Left errMsg -> putStrLn $ "Error parsing wasm file: " ++ errMsg
        Right wasmModule -> putStrLn $ "Parsed wasm module:\n" ++ show wasmModule  -- Display the parsed module
