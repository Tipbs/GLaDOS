module Wasm () where
import Parser (LispVal (..))
import Numeric (showHex)
import Data.Binary (encode, Word8)
import qualified Data.ByteString.Lazy as BL

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

-- in wasm numbers are as small as possible in memory, for example 15 would only take one byte [0x0f]
buildNumber :: Int -> [Word8]
buildNumber nb = wordClean $ BL.unpack $ encode nb
    where
        wordClean :: [Word8] -> [Word8]
        wordClean [0] = [0]
        wordClean (0: xs) = wordClean xs
        wordClean v = v

-- [104,101,108,108,111]

buildVarAssign :: LispVal -> [Int]
buildVarAssign (List [Atom "assign", String name, Number val]) = []
buildVarAssign (List [Atom "assign", String name, String val]) = []
buildVarAssign (List [Atom "assign", String name, Bool val]) = []
buildVarAssign (List [Atom "assign", String name, val]) = []
buildVarAssign (List [Atom "assign", String name, val]) = []
buildVarAssign _ = []

compileNumber :: Int -> [Word8]
compileNumber val = wasmOpToCode I32const ++ buildNumber val

compileGetLocalVar :: String -> [(String, Int)] -> [Word8]
compileGetLocalVar localVar localList = case mIndex of
    Nothing -> []
    Just i -> 0x20 : buildNumber i
    where
        mIndex = lookup localVar localList

compileExpr :: LispVal -> [LispVal] -> [(String, Int)] -> [LispVal] -> ([Word8], [(String, Int)], [LispVal])
compileExpr (Number val) funcs locals stack = (compileNumber val, locals, stack)
compileExpr (Atom localVar) funcs locals stack = (compileGetLocalVar localVar locals, locals, stack)

buildWasm :: [Word8]
buildWasm = magic ++ version
