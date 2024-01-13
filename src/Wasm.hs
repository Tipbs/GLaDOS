module Wasm (buildWasm, compileExpr, magic, version, buildSectionHeader, buildDataSec, buildDataSegments, buildSegmentHeader, getIdData, compileOp, WasmOp (..)) where
import KopeParserLib (KopeVal (..))
import Numeric (showHex)
import Control.Monad (liftM, foldM)
import Data.Binary (Word8)
import WasmNumber (buildNumber, buildString)
import Data.Either (isRight)
import Control.Monad.Except (MonadError(throwError))

data WasmOp = LocalSet Int | LocalGet Int | I32add | I32sub | I32mul | I32div | I32const | EndFunc | Return
    deriving (Eq)

type Stack = [KopeVal]
type Local = (String, Int)
type Data = KopeVal

-- https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/Numeric
wasmOpToCode :: WasmOp -> [Word8]
wasmOpToCode (LocalSet val) = 0x21 : buildNumber val
wasmOpToCode (LocalGet val) = 0x20 : buildNumber val
wasmOpToCode I32add = [0x6a]
wasmOpToCode I32sub = [0x6b]
wasmOpToCode I32mul = [0x6c]
wasmOpToCode I32div = [0x6d]
wasmOpToCode I32const = [0x41]
wasmOpToCode EndFunc = [0x0b]
wasmOpToCode Return = []

magic :: [Word8]
magic = [0x00, 0x61, 0x73, 0x6d]

version :: [Word8]
version = [0x01, 0x00, 0x00, 0x00]

buildSectionHeader :: Word8 -> Int -> Int -> [Word8]
buildSectionHeader code size nb = [code] ++ buildNumber newSize ++ newNb
    where
        newNb = buildNumber nb
        newSize = size + length newNb

-- if the signature already exist it should not push the result
buildFunctionType :: KopeVal -> [Word8] -- pour le moment le type est forcé i32
buildFunctionType (KopeFunc _ p _) = [0x60] ++ buildNumber (length p) ++ map (const 0x7f) p ++ [0x01, 0x7f] -- le dernier tableau correspondrait au return
buildFunctionType _ = []

-- ghci > debugHex $ buildSectionType [Func "add" ["15", "5"] [Number 5]]
buildSectionType :: [KopeVal] -> [Word8]
buildSectionType functions = buildSectionHeader 0x01 section_size (length functions) ++ concat functions_types
    where
        functions_types = map buildFunctionType functions
        concated = concat functions_types
        section_size = length concated

debugHex :: [Word8] -> [String]
debugHex = map (`showHex` "")

buildFunctionSec :: [KopeVal] -> [Word8]
buildFunctionSec functions = buildSectionHeader 0x03 section_size (length functions) ++ concated
    where
        function_index = map buildNumber [0..length functions - 1]
        concated = concat function_index
        section_size = length concated

compileNumber :: Int -> [Word8]
compileNumber val = wasmOpToCode I32const ++ buildNumber val

compileGetLocalVar :: String -> [Local] -> [Data] -> Either String ([Word8], [(String, Int)], [Data])
compileGetLocalVar localVar localList datas = case mIndex of
    Nothing -> Left $ "Couldn't find local variable " ++ localVar
    Just i -> Right (wasmOpToCode $ LocalGet i, localList, datas)
    where
        mIndex = lookup localVar localList

compileOp :: String -> [Word8]
compileOp str = maybe [] wasmOpToCode opeOp
    where
        opeOp = lookup str primitives

primitives :: [(String, WasmOp)]
primitives = [("+", I32add),
              ("-", I32sub),
              ("*", I32mul),
              ("/", I32div),
              ("return", Return)
            ]

getIdFunction :: Int -> String -> [KopeVal] -> Maybe Int
getIdFunction _ _ [] = Nothing
getIdFunction len called (KopeFunc func _ _ : rest)
    | isMatching = Just len
    | otherwise = getIdFunction (len + 1) called rest
    where
        isMatching = called == func
getIdFunction _ _ _ = Nothing

getFunctionCall :: String -> [KopeVal] -> Either String [Word8]
getFunctionCall called funcs = case id_function of
    Just i -> Right $ 0x10 : buildNumber i
    Nothing -> Left $ "Could not find function named " ++ called
    where
        id_function = getIdFunction 0 called funcs

buildSegmentHeader :: Int -> [Word8]
buildSegmentHeader idx = [0x00, 0x41] ++ buildNumber idx ++ [0x0b]

getIdData :: Int -> Data -> [Data] -> Int
getIdData len (KopeString func) (KopeString x:datas)
    | func == x = len
    | otherwise = getIdData (len + 1) (KopeString func) datas

getSegDataSize :: Data -> Int
getSegDataSize (KopeString func) = length (buildString func)

buildDataSegments :: Data -> [Data] -> [Word8]
buildDataSegments (KopeString func) datas = buildSegmentHeader id_data ++ buildNumber segLen ++ buildString func 
    where
        id_data = getIdData 0 (KopeString func) datas
        segLen = getSegDataSize (KopeString func)


buildDataSec :: [Data] -> [Word8]
buildDataSec datas = buildSectionHeader 0x0b section_size (length datas) ++ concated
    where
        data_segments = map (`buildDataSegments` datas) datas
        concated = concat data_segments
        section_size = length concated

compileFunctionBody :: KopeVal -> [KopeVal] -> [Data] -> Either String ([Word8], [Local], [Data])
compileFunctionBody (KopeFunc _ ps bod) funcs oldDatas = case evaledFunc of
    (Right (bytes, locals, datas)) ->
        let local_decl_count = buildNumber (length locals - length ps)
            header = buildNumber (length bytes + 1 + length local_decl_count) ++ local_decl_count -- +1 for end
            in Right (header ++ bytes ++ wasmOpToCode EndFunc, locals, datas ++ oldDatas)
    v@(Left _) -> v
    where
        mapWithIndex :: [String] -> Int -> [(String, Int)]
        mapWithIndex [] _ = []
        mapWithIndex (x: xs) i = (x, i) : mapWithIndex xs (i + 1)
        paramToLocals = mapWithIndex ps 0
        evaledFunc = foldM (\(bytes, locals, datas) expr ->
            case compileExpr expr funcs locals [] of
                Right (cBytes, cLocals, cDatas) -> return (bytes ++ cBytes, cLocals, cDatas ++ datas)
                Left err -> throwError err
            ) ([], paramToLocals, []) bod
compileFunctionBody _ _ _ = Left "Invalid call to compileFunctionBody"

-- debugHex $ fst (compileExpr (List [Atom "add", Number 5]) [Func "add" ["a", "b"] [Number 5], Func "sub" ["a", "b"] [Number 5]] [])
compileExpr :: KopeVal -> Stack -> [Local] -> [Data] -> Either String ([Word8], [Local], [Data])
compileExpr f@(KopeFunc {}) funcs _ datas = compileFunctionBody f funcs datas
compileExpr (KopeArray [KopeAtom "define", KopeAtom var, KopeNumber form]) _ locals datas = Right ([0x01, 0x7f], locals ++ [(var, form)], datas)
compileExpr (KopeNumber val) _ locals datas = Right (compileNumber val, locals, datas)
compileExpr (KopeBool val) _ locals datas = Right (compileNumber nbVal, locals, datas)
    where
        nbVal = if val then 1 else 0
compileExpr (KopeAtom localVar) _ locals datas = compileGetLocalVar localVar locals datas
compileExpr (KopeArray (KopeAtom func : args)) funcs locals datas = case checked of
    Right (argsDat, callB) -> Right (concatMap (\(b, _, _) -> b) argsDat ++ callB, locals, concatMap (\(_, _, p) -> p) argsDat ++ datas)
    (Left err) -> Left err
    where
        argsB = mapM (\arg -> compileExpr arg funcs locals []) args
        checkBoth (Right arg) (Right funcCall) = Right (arg, funcCall)
        checkBoth (Left argErr) _ = Left argErr
        checkBoth _ (Left callErr) = Left callErr
        maybeToEither Nothing = Left ""
        maybeToEither (Just val) = Right val
        primitiveFunc = wasmOpToCode <$> maybeToEither (lookup func primitives)
        functionCall = if isRight primitiveFunc then primitiveFunc else getFunctionCall func funcs
        checked = checkBoth argsB functionCall
compileExpr _ _ _ _ = Left "Not defined yet"
        -- concated = beforeB ++ getFunctionCall func funcs

-- code horrible
buildSectionBody :: [KopeVal] -> Either String ([Word8], [Data])
buildSectionBody funcs = combineEither concatedB concatedD
    where
        compiled = mapM (\func -> compileExpr func funcs [] []) funcs
        mapped = map (\(b, _, d) -> (b, d)) <$> compiled
        concatedB = concatMap fst <$> mapped
        concatedD = concatMap snd <$> mapped
        combineEither :: Either String [Word8] -> Either String [Data] -> Either String ([Word8], [Data])
        combineEither (Right b) (Right d) = Right (buildSectionHeader 0x0a (length b) (length funcs) ++ b, d)
        combineEither (Left err) _ = Left err
        combineEither _ (Left err) = Left err

buildWasm :: [KopeVal] -> Either String [Word8]
buildWasm funcs = case bo of
    Right (bodyBytes, _) -> Right $ magic ++ version ++ buildSectionType funcs ++ buildFunctionSec funcs ++ bodyBytes
    (Left err) -> Left err
    where
        bo = buildSectionBody funcs
