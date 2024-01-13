{-# HLINT ignore "Use newtype instead of data" #-}
module WASMParser (wasmParser) where
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word
import WasmNumber (decodeNumber)
import Data.Bits ((.&.))
import Control.Monad (replicateM)

data WasmModule = WasmModule {
    wasmFuncs :: [WasmFunction],
    wasmFuncBodies :: [[Word8]]
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

parseSectionType :: Get ()
parseSectionType = do
    code <- getWord8
    case code of
        0x01 -> do
            sec_len <- parseNumber
            skip sec_len
        _ -> fail "Section type wasn't found after version"

parseFunctionCode :: Get [Word8]
parseFunctionCode = do
    len <- parseNumber
    replicateM len getWord8

parseSectionCode :: Get [[Word8]]
parseSectionCode = do
    code <- getWord8
    case code of
        0x0a -> do
            func_count <- parseNumber
            replicateM func_count parseFunctionCode
        _ -> fail "Section code isn't after section fonction (could be normal)"


parseWasmModule :: Get WasmModule
parseWasmModule = do
    magic <- getWord32be
    version <- getWord32be
    if magic /= 0x0061736d || version /= 0x01
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
