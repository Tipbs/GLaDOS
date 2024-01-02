module Wasm () where
import Parser (LispVal (..))

magic :: [Integer]
magic = [0x00, 0x61, 0x73, 0x6d]

version :: [Integer]
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

buildFunctionType :: LispVal -> [Int] -- pour le moment le type est forcé i32
buildFunctionType (Func n p _) = [0x60, length p] ++ map (const 0x7f) p ++ [0x01, 0x7f] -- le dernier tableau correspondrait au return
buildFunctionType _ = []

buildWasm :: [Integer]
buildWasm = magic ++ version