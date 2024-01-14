{-# HLINT ignore "Use newtype instead of data" #-}
module WASMParser (wasmParser, WasmModule (..), WasmFunction (..)) where
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word
import WasmNumber (decodeNumber)
import Data.Bits ((.&.))
import Control.Monad (replicateM)
import Data.Char (chr)

data WasmModule = WasmModule {
    wasmFuncs :: [WasmFunction],
    wasmFuncBodies :: [([Word8], Int)] -- bytes, local_decl_count (should probably be nbParams + local decl count)
}

data WasmFunction = WasmFunction {
    nbParams :: Int,
    name :: [Char]
    -- paramsType :: [ParamsType]
}

data ParamsType = I32 | I64

instance Show WasmModule where
    show (WasmModule functions bodies) = "Functions: " ++ show functions ++ "\nBodies: " ++ show bodies

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

validWasmFunction :: [WasmFunction] -> [WasmFunction]
validWasmFunction [] = []
validWasmFunction (x:xs)
    | nbParams x == 0 = validWasmFunction xs
    | otherwise = x : validWasmFunction xs

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

parseBody :: Get [Word8]
parseBody = do
    byte <- getWord8
    if byte == 0x0b -- end opcode
        then return [byte]
        else do
            rest <- parseBody
            return (byte : rest)

parseFunctionCode :: Get ([Word8], Int)
parseFunctionCode = do
    len <- parseNumber
    decl_count <- parseDeclCount
    body <- parseBody
    return (body, decl_count)

parseSectionCode :: Get [([Word8], Int)]
parseSectionCode = do
    code <- getWord8
    case code of
        0x0a -> do
            size <- parseNumber
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
            size <- parseNumber
            num_funcs <- parseNumber
            replicateM num_funcs parseFuncType
        _ -> fail "The program didn't start with section type"

parseSectionFunction :: Get [Int] -- return signature index
parseSectionFunction = do
    code <- getWord8
    case code of
        0x03 -> do
            size <- parseNumber
            num_funcs <- parseNumber
            replicateM num_funcs parseNumber
        _ -> fail "Section Type was not followed with section function"

parseSectionExport :: Get [(Int, [Char])]
parseSectionExport = do
    code <- getWord8
    case code of
        0x07 -> do
            _ <- parseNumber
            nb_exports <- parseNumber
            replicateM nb_exports parseString
        _ -> fail "Export section was not found after section function (mandatory to find main)"
    where
        parseString :: Get (Int, [Char])
        parseString = do
            len <- parseNumber
            string <- replicateM len getWord8
            export_kind <- getWord8
            index <- parseNumber
            case export_kind of
                0x00 -> return (index, map (chr . fromIntegral) string)
                0x02 -> return (-1, map (chr . fromIntegral) string)
                _ -> fail "Unknown export type"


parseSections :: Get WasmModule
parseSections = do
    type_sec <- parseSectionType
    func_sec <- parseSectionFunction
    exports <- parseSectionExport
    let functions_params = map (type_sec !!) func_sec
    -- error $ show exports
    let functions_strings = mapM (`lookup` exports) [0..length functions_params - 1] -- fetch name for each func
    case functions_strings of
        Just names -> do
            let functions = map (\(n, p_count) -> WasmFunction {nbParams = p_count, name = n}) (zip names functions_params)
            bodies <- parseSectionCode
            return WasmModule {wasmFuncs = functions, wasmFuncBodies = bodies}
        _ -> fail "One function had no export which match its index"


parseWasmModule :: Get WasmModule
parseWasmModule = do
    magic <- getWord32be
    version <- getWord32le
    if magic /= 0x0061736d || version /= 1
        then
            fail "Wrong magic or version number"
        else do
            parseSections

parseWasmFile :: FilePath -> IO (Either String WasmModule)
parseWasmFile filePath = do
    fileContent <- BL.readFile filePath

    case runGetOrFail parseWasmModule fileContent of
        Left (_, _, errMsg) -> return $ Left errMsg
        Right (_, _, wasmModule) -> return $ Right wasmModule

wasmParser :: String -> IO (Either String WasmModule)
wasmParser wasmFilePath = do
    parsedResult <- parseWasmFile wasmFilePath

    case parsedResult of
        Left errMsg -> return $ Left errMsg
        Right wasmModule -> return $ Right wasmModule
