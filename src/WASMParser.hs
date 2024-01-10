module WASMParser () where
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Int

data WasmModule = WasmModule {
    wasmFuncs :: [WasmFunction]
}

data WasmFunction = WasmFunction {
    nbParams :: Integer,
    paramsType :: [ParamsType]
}

data ParamsType = I32 | I64

typeStr :: ParamsType -> String
typeStr I32 = "i32"
typeStr I64 = "i64"

instance Show WasmModule where
    show (WasmModule functions) = "Functions: " ++ show functions

instance Show WasmFunction where
    show (WasmFunction nbParams paramsType) =
        "nbParams: " ++ show nbParams ++
        "paramsType: " ++ show paramsType

instance Show ParamsType where
    show I32 = "i32"
    show I64 = "i64"

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
