module Wasm () where
import Parser (LispVal (..))
import Numeric (showHex)
import Data.Binary (encode, Word8, Word32)
import qualified Data.ByteString.Lazy as BL
import Data.Bits (Bits(shift, shiftR, (.&.), (.|.), shiftL))
import Control.Monad (foldM)
import Control.Monad (liftM)

data WasmOp = LocalSet Int | LocalGet Int | I32add | I32sub | I32const
    deriving (Eq)

wasmOpToCode :: WasmOp -> [Word8]
wasmOpToCode (LocalSet val) = 0x21 : buildNumber val
wasmOpToCode (LocalGet val) = 0x20 : buildNumber val
wasmOpToCode I32add = [0x6a]
wasmOpToCode I32sub = [0x6b]
wasmOpToCode I32const = [0x41]

magic :: [Word8]
magic = [0x00, 0x61, 0x73, 0x6d]

version :: [Word8]
version = [0x01, 0x00, 0x00, 0x00]

-- 0000000: 0061 736d                                 ; WASM_BINARY_MAGIC
-- 0000004: 0100 0000                                 ; WASM_BINARY_VERSION
-- ; section "Type" (1)
-- 0000008: 01                                        ; section code
-- 0000009: 07                                        ; section size (guess)
-- 000000a: 01                                        ; num types
-- ; func type 0
-- 000000b: 60                                        ; func
-- 000000c: 02                                        ; num params
-- 000000d: 7f                                        ; i32
-- 000000e: 7f                                        ; i32
-- 000000f: 01                                        ; num results
-- 0000010: 7f                                        ; i32
-- ; section "Function" (3)
-- 0000011: 03                                        ; section code
-- 0000012: 02                                        ; section size (guess)
-- 0000013: 01                                        ; num functions
-- 0000014: 00                                        ; function 0 signature index
-- ; section "Code" (10)
-- 0000015: 0a                                        ; section code
-- 0000016: 09                                        ; section size (guess)
-- 0000017: 01                                        ; num functions
-- ; function body 0
-- 0000018: 07                                        ; func body size (guess)
-- 0000019: 00                                        ; local decl count
-- 000001a: 20                                        ; local.get
-- 000001b: 00                                        ; local index
-- 000001c: 20                                        ; local.get
-- 000001d: 01                                        ; local index
-- 000001e: 6a                                        ; i32.add
-- 000001f: 0b                                        ; end

-- https://webassembly.github.io/spec/core/binary/modules.html

-- fn add(a, b)
-- {
--     var c = a + b;
--     return c;
-- }

-- tableau local [a, b, ?]
-- stack []

-- Section Code, function body 0:
-- 0d ; body byte len
-- 01 ; local decl count (nombre de declarations de type)
-- 01 ; local type count
-- 7f ; i32
-- 20 ; local.get
-- 00 ; local index
-- 20 ; local.get
-- 01 ; local index ; stack = [a, b]
-- 6a ; i32.add ; stack = [a+b]
-- 21 ; local.set
-- 02 ; local index ; stack = []
-- 20 ; local.get
-- 02 ; stack = [c]
-- 0b ; end

-- ; section "Type" (1)
-- 0000008: 01                                        ; section code
-- 0000009: 07                                        ; section size (guess)
-- 000000a: 01                                        ; num types
-- ; func type 0
-- 000000b: 60                                        ; func
-- 000000c: 02                                        ; num params
-- 000000d: 7f                                        ; i32
-- 000000e: 7f                                        ; i32
-- 000000f: 01                                        ; num results
-- 0000010: 7f                                        ; i32

buildSectionHeader :: Word8 -> Int -> Int -> [Word8]
buildSectionHeader code size nb = [code] ++ buildNumber size ++ buildNumber nb

-- if the signature already exist it should not push the result
buildFunctionType :: LispVal -> [Word8] -- pour le moment le type est forcé i32
buildFunctionType (Func _ p _) = [0x60] ++ buildNumber (length p) ++ map (const 0x7f) p ++ [0x01, 0x7f] -- le dernier tableau correspondrait au return
buildFunctionType _ = []

-- ghci > debugHex $ buildSectionType [Func "add" ["15", "5"] [Number 5]]
buildSectionType :: [LispVal] -> [Word8]
buildSectionType functions = buildSectionHeader 0x01 section_size (length functions) ++ concat functions_types
    where
        functions_types = map buildFunctionType functions
        concated = concat functions_types
        section_size = length concated + 1 -- + 1 cause num types is in section size

debugHex :: [Word8] -> [String]
debugHex = map (`showHex` "")

-- ; section "Function" (3)
-- 0000011: 03                                        ; section code
-- 0000012: 02                                        ; section size (guess)
-- 0000013: 01                                        ; num functions
-- 0000014: 00                                        ; function 0 signature index
-- should get all functions and assignate them to their index
--
buildFunctionSec :: [LispVal] -> [Word8]
buildFunctionSec functions = buildSectionHeader 0x03 section_size (length functions) ++ concated
    where
        function_index = map buildNumber [0..length functions - 1]
        concated = concat function_index
        section_size = length concated + 1

buildWords :: Int -> [Word8]
buildWords 0 = []
buildWords nb = rmLastB : buildWords shifted
    where 
        rmLastB = (.&.) (fromIntegral nb :: Word8) 0x7f
        shifted = shiftR nb 7
        
-- https://en.wikipedia.org/wiki/LEB128
buildNumber:: Int -> [Word8]
buildNumber nb = setHighestByte
    where
        sepBySeven = buildWords nb
        tailBitSet = map (.|. 128) (tail sepBySeven)
        setHighestByte = head sepBySeven : tailBitSet

buildVarAssign :: LispVal -> [Int]
buildVarAssign (List [Atom "assign", String name, Number val]) = []
buildVarAssign (List [Atom "assign", String name, String val]) = []
buildVarAssign (List [Atom "assign", String name, Bool val]) = []
buildVarAssign (List [Atom "assign", String name, val]) = []
buildVarAssign (List [Atom "assign", String name, val]) = []
buildVarAssign _ = []

compileNumber :: Int -> [Word8]
compileNumber val = wasmOpToCode I32const ++ buildNumber val

compileGetLocalVar :: String -> [(String, Int)] -> Either String ([Word8], [(String, Int)])
compileGetLocalVar localVar localList = case mIndex of
    Nothing -> Left $ "Couldn't find local variable " ++ localVar
    Just i -> Right (wasmOpToCode $ LocalGet i, localList)
    where
        mIndex = lookup localVar localList

compileOp :: String -> [Word8]
compileOp str = maybe [] wasmOpToCode opeOp
    where
        opeOp = lookup str primitives

primitives :: [(String, WasmOp)]
primitives = [("+", I32add),
              ("-", I32sub)
            ]

getIdFunction :: Int -> String -> [LispVal] -> Maybe Int
getIdFunction _ called [] = Nothing
getIdFunction len called (Func func _ _ : rest)
    | isMatching = Just len
    | otherwise = getIdFunction (len + 1) called rest
    where
        isMatching = called == func
getIdFunction _ _ _ = Nothing

getFunctionCall :: String -> [LispVal] -> Either String [Word8]
getFunctionCall called funcs = case id_function of
    Just i -> Right $ 0x10 : buildNumber i
    Nothing -> Left $ "Could not find function named " ++ called
    where
        id_function = getIdFunction 0 called funcs

buildFunctionBody :: LispVal -> [LispVal] -> Either String ([Word8], [(String, Int)])
buildFunctionBody (Func name params body) funcs = Left "temp"
    where
        evaledFunc = foldM (\acc expr -> compileExpr expr funcs (snd acc)) ([], []) body
buildFunctionBody _ _ = Left "Invalid call to buildFunctionBody"

-- debugHex $ fst (compileExpr (List [Atom "add", Number 5]) [Func "add" ["a", "b"] [Number 5], Func "sub" ["a", "b"] [Number 5]] [])
compileExpr :: LispVal -> [LispVal] -> [(String, Int)] -> Either String ([Word8], [(String, Int)])
compileExpr (Func name params form) funcs locals = case argsB of
    Right [(_, newLocals)] -> Right (buildNumber (length newLocals), locals)
    (Left err) -> Left err
    where
        argsB = mapM (\arg -> compileExpr arg funcs locals) form
compileExpr (List [Atom "define", Atom var, Number form]) funcs locals = Right ([], [(var, form)])
compileExpr (Number val) _ locals = Right (compileNumber val, locals)
compileExpr (Bool val) _ locals = Right (compileNumber nbVal, locals)
    where
        nbVal = if val then 1 else 0
compileExpr (Atom localVar) _ locals = compileGetLocalVar localVar locals
compileExpr (List (Atom func : args)) funcs locals = case checked of
    Right (argB, callB) -> Right (concatMap fst argB ++ callB, locals)
    (Left err) -> Left err
    where
        argsB = mapM (\arg -> compileExpr arg funcs locals) args
        checkBoth (Right arg) (Right funcCall) = Right (arg, funcCall)
        checkBoth (Left argErr) _ = Left argErr
        checkBoth _ (Left callErr) = Left callErr
        functionCall = getFunctionCall func funcs
        checked = checkBoth argsB functionCall
compileExpr _ _ _ = Left "Not defined yet"
        -- concated = beforeB ++ getFunctionCall func funcs

buildWasm :: [Word8]
buildWasm = magic ++ version
