{-# HLINT ignore "Use newtype instead of data" #-}
module WASMParser () where
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Int

data WasmModule = WasmModule {
    wasmFuncs :: [WasmFunction]
}

data WasmFunction = WasmFunction {
    nbParams :: Word8,
    paramsType :: [ParamsType]
}

data ParamsType = UNKNOWN | I32 | I64

typeStr :: ParamsType -> String
typeStr UNKNOWN = "Unknown"
typeStr I32 = "i32"
typeStr I64 = "i64"

instance Show WasmModule where
    show (WasmModule functions) = "Functions: " ++ show functions

instance Show WasmFunction where
    show (WasmFunction paramsNb typeParams) =
        "\n{\n    nbParams: " ++ show paramsNb ++ "\n" ++
        "    paramsType: " ++ show typeParams ++ "\n}\n"

instance Show ParamsType where
    show UNKNOWN = "Unknown"
    show I32 = "i32"
    show I64 = "i64"

getWasmType :: Integer -> [ParamsType] -> Get [ParamsType]
getWasmType n typesParams = do
    if n == 0
        then
            return typesParams
        else do 
            byte <- getWord8
            paramType <- case byte of
                0x7f -> return I32
                0x7e -> return I64
                _ -> return UNKNOWN
            getWasmType (n - 1) (typesParams ++ [paramType])

parseWasmFunctions :: Bool -> Get WasmFunction
parseWasmFunctions doParse = do
    if not doParse
        then
            return WasmFunction { nbParams = 0, paramsType = []}
        else do
            paramsNb <- getWord8
            params <- getWasmType (fromIntegral paramsNb) []
            return WasmFunction { nbParams = paramsNb, paramsType = params }

validWasmFunction :: [WasmFunction] -> [WasmFunction]
validWasmFunction [] = []
validWasmFunction (x:xs)
    | nbParams x == 0 = validWasmFunction xs
    | otherwise = x : validWasmFunction xs

parseWasmBytes :: WasmModule -> Get WasmModule
parseWasmBytes wasmModule = do
    empty <- isEmpty
    if empty
        then
            return WasmModule { wasmFuncs = validWasmFunction (wasmFuncs wasmModule) }
        else do
            byte <- getWord8
            funcs <- case byte of
                0x60 -> parseWasmFunctions True
                _ -> parseWasmFunctions False
            parseWasmBytes WasmModule { wasmFuncs = wasmFuncs wasmModule ++ [funcs]}

parseWasmModule :: Get WasmModule
parseWasmModule = do
    magic <- getWord32be
    if magic /= 0x0061736d
        then
            fail "Wrong magic number"
        else do
            parseWasmBytes WasmModule { wasmFuncs = []}

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
