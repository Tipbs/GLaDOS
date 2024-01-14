{-# HLINT ignore "Use newtype instead of data" #-}
module WASMParser (wasmParser, WasmModule (..), WasmFunction (..)) where
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word
import WasmNumber (decodeNumber)
import Data.Bits ((.&.))
import Control.Monad (replicateM)

data WasmModule = WasmModule {
    wasmFuncs :: [WasmFunction],
    wasmFuncBodies :: [([Word8], Int)] -- bytes, local_decl_count (should probably be nbParams + local decl count)
}

data WasmFunction = WasmFunction {
    nbParams :: Int,
    paramsType :: [ParamsType]
}

data ParamsType = I32 | I64

instance Show WasmModule where
    show (WasmModule functions bodies) = "Functions: " ++ show functions ++ show bodies

instance Show WasmFunction where
    show (WasmFunction paramsNb typeParams) =
        "\n{\n    nbParams: " ++ show paramsNb ++ "\n" ++
        "    paramsType: " ++ show typeParams ++ "\n}\n"

instance Show ParamsType where
    show I32 = "i32"
    show I64 = "i64"

getWasmType :: Int -> [ParamsType] -> Get [ParamsType]
getWasmType n typesParams = do
    if n == 0
        then
            return typesParams
        else do
            byte <- getWord8
            paramType <- case byte of
                0x7f -> return I32
                0x7e -> return I64
                _ -> return I32
            getWasmType (n - 1) (typesParams ++ [paramType])

parseLebWords :: Get [Word8]
parseLebWords = do
    byte <- getWord8
    if byte .&. 128 == 0
        then return [byte]
        else do
            rest <- parseLebWords
            return (byte : rest)

parseNumber :: Get Int
parseNumber = decodeNumber <$> parseLebWords

parseWasmFunctions :: Bool -> Get WasmFunction
parseWasmFunctions doParse = do
    if not doParse
        then
            return WasmFunction { nbParams = 0, paramsType = []}
        else do
            paramsNb <- parseNumber
            params <- getWasmType paramsNb []
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
            return WasmModule { wasmFuncs = validWasmFunction (wasmFuncs wasmModule), wasmFuncBodies = []}
        else do
            byte <- getWord8
            funcs <- case byte of
                0x60 -> parseWasmFunctions True
                _ -> parseWasmFunctions False
            parseWasmBytes WasmModule { wasmFuncs = wasmFuncs wasmModule ++ [funcs], wasmFuncBodies = []}

parseDeclCount :: Get Int
parseDeclCount = do
    local_decl_count <- parseNumber
    parsed <- replicateM local_decl_count parseType
    return $ sum parsed
    where
        parseType :: Get Int
        parseType = do
            count <- parseNumber
            skip 1
            return count

parseFunctionCode :: Get ([Word8], Int)
parseFunctionCode = do
    len <- parseNumber
    decl_count <- parseDeclCount
    body <- replicateM len getWord8
    return (body, decl_count)

parseSectionCode :: Get [([Word8], Int)]
parseSectionCode = do
    code <- getWord8
    case code of
        0x0a -> do
            func_count <- parseNumber
            replicateM func_count parseFunctionCode
        _ -> fail "Section code isn't after section fonction (could be normal)"

parseFuncType :: Get Int
parseFuncType = do
    code <- getWord8
    case code of
        0x60 -> do
            num_params <- parseNumber
            skip 2
            num_results <- parseNumber
            skip num_results
            return num_params
        _ -> fail "A type didn't start with 0x60 (func type)"

parseSectionType :: Get [Int]
parseSectionType = do
    code <- getWord8
    case code of
        0x01 -> do
            num_funcs <- parseNumber
            replicateM num_funcs parseFuncType
        _ -> fail "The program didn't start with section type"

parseWasmModule :: Get WasmModule
parseWasmModule = do
    magic <- getWord32be
    version <- getWord32le
    if magic /= 0x0061736d || version /= 1
        then
            fail "Wrong magic or version number"
        else do
            parseWasmBytes WasmModule {wasmFuncs = [], wasmFuncBodies = []}

parseWasmFile :: FilePath -> IO (Either String WasmModule)
parseWasmFile filePath = do
    fileContent <- BL.readFile filePath

    case runGetOrFail parseWasmModule fileContent of
        Left (_, _, errMsg) -> return $ Left errMsg
        Right (_, _, wasmModule) -> return $ Right wasmModule

wasmParser :: String -> IO ()
wasmParser wasmFilePath = do
    parsedResult <- parseWasmFile wasmFilePath

    case parsedResult of
        Left errMsg -> putStrLn $ "Error parsing wasm file: " ++ errMsg
        Right wasmModule -> putStrLn $ "Parsed wasm module:\n" ++ show wasmModule
