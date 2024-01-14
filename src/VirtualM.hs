{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module VirtualM () where

import Data.Word (Word8)
import Data.Bits ( Bits((.&.)) )
import WasmNumber (decodeNumber)
import WASMParser (WasmModule (WasmModule, wasmFuncs), WasmFunction (WasmFunction))

type Stack = [Value]
type Local = [Value]
type Func = String
type Insts = [Word8]
type Args = [Value]
type Funcs = ([Word8], Int)

type Value = Int

data Builtin = Add
             | Sub
             | Mult
             | Div
             | Eq
             | Less
             | High
             deriving(Eq, Show, Read)

data Instructions = Push Value
                  | PushArg Int
                  | Call
                  | JumpIfFalse Int
                  | JumpIfTrue Int
                  | Ret
                 deriving(Eq, Show, Read)

addi :: Stack -> Either String Stack
addi [] = Left "Error : Not enough arguments in Add."
addi [_] = Left "Error : Not enough arguments in Add."
addi (a: b: c) = Right (a + b: c)

subs :: Stack -> Either String Stack
subs [] = Left "Error : Not enough arguments in Sub."
subs [_] = Left "Error : Not enough arguments in Sub."
subs (a: b: c) = Right ((a - b):c)

mult :: Stack -> Either String Stack
mult [] = Left "Error : Not enough arguments in Mult."
mult [_] = Left "Error : Not enough arguments in Mult."
mult (a: b: c) = Right ((a * b):c)

-- eq :: Stack -> Either String Stack
-- eq [] = Left "Error : Not enough arguments in Eq."
-- eq [_] = Left "Error : Not enough arguments in Eq."
-- eq (a: b: c) = Right ((a == b):c)
-- eq _ = Left "Error : One argument is a wrong type in Eq."

-- less :: Stack -> Either String Stack
-- less [] = Left "Error : Not enough arguments in Less."
-- less [_] = Left "Error : Not enough arguments in Less."
-- less (a: b: c) = Right (Boolean (a < b):c)
-- less _ = Left "Error : One argument is a wrong type in Less."

-- high :: Stack -> Either String Stack
-- high [] = Left "Error : Not enough arguments in High."
-- high [_] = Left "Error : Not enough arguments in High."
-- high (a: b: c) = Right (Boolean (a > b):c)
-- high _ = Left "Error : One argument is a wrong type in High."

divi :: Stack -> Either String Stack
divi [] = Left "Error : Not enough arguments in Div."
divi [_] = Left "Error : Not enough arguments in Div."
divi (_: 0: _) = Left "Error : Divison by 0."
divi (a: b: c) = Right (a `div` b : c)

push :: Value -> Stack -> Stack
push val st = val:st

pusharg :: Args -> Int -> Stack -> Either String Stack
pusharg [] _ _ = Left "Error : Not enough arguments."
pusharg (a:_) 0 st = Right (a:st)
pusharg (_:b) num st = pusharg b (num - 1) st

vmParseNumber :: [Word8] -> (Int, [Word8])
vmParseNumber w = (decoded, drop (length parsed) w)
    where
        parseUntilHighBit :: [Word8] -> [Word8]
        parseUntilHighBit (word: xs) | (word .&. 128) /= 0 = word : parseUntilHighBit xs
        parseUntilHighBit _ = []
        parsed = parseUntilHighBit w
        decoded = decodeNumber parsed

primitives :: [(Word8, Int -> Int -> Int)]
primitives = [(0x6a, (+)),
              (0x6b, (-)),
              (0x6c, (*)),
              (0x6d, div)
            ]

execPrimitive :: Word8 -> Stack -> Either String Stack
execPrimitive op (a: b: xs) = case mOperation of
    Just operation -> Right $ a `operation` b: xs
    Nothing -> Left $ "Wrong operator: " ++ show op
    where
        mOperation = lookup op primitives
execPrimitive _ _ = Left "Wrong call to execPrimitive"

-- execPrimitive :: Insts -> Stack -> Stack
-- execPrimitive (op: remain) stack = firstVal `operation` secondVal
--     where
--         operation = lookup op primitives
--         (firstVal, firstRemain) = vmParseNumber remain
--         (secondVal, secondRemain) = vmParseNumber secondRemain

exec :: Insts -> Stack -> Local -> WasmModule -> Either String Value
exec [] _ _ _ = Left "Error : No return instruction."
exec [0xb] [val] _ _ = Right val -- end 
exec (0x41: remain) st locals funcs = exec remaining (push nb st) locals funcs
    where
        (nb, remaining) = vmParseNumber remain
exec (0x6a : remain) stack locals funcs = do
                                      let res = execPrimitive 0x6a stack
                                      case res of
                                        Right inst -> exec remain inst locals funcs
                                        Left str -> Left str
exec (0x6b : remain) stack locals funcs = do
                                      let res = execPrimitive 0x6b stack
                                      case res of
                                        Right inst -> exec remain inst locals funcs
                                        Left str -> Left str
exec (0x6c : remain) stack locals funcs = do
                                      let res = execPrimitive 0x6c stack
                                      case res of
                                        Right inst -> exec remain inst locals funcs
                                        Left str -> Left str
exec (0x6d : remain) stack locals funcs = do
                                      let res = execPrimitive 0x6d stack
                                      case res of
                                        Right inst -> exec remain inst locals funcs
                                        Left str -> Left str
exec (0x21: remain) (val: sRemain) locals funcs = exec remaining sRemain locals_updated funcs
    where
        (index, remaining) = vmParseNumber remain
        replace :: Local -> Int -> Value -> Local
        replace (_: xs) 0 v = v: replace xs (-1) v
        replace (x: xs) i v = x: replace xs (i - 1) v
        replace _ _ _ = []
        locals_updated = replace locals index val
exec (0x20: remain) stack locals funcs = exec remaining (val: stack) locals funcs
    where
        (index, remaining) = vmParseNumber remain
        val = locals !! index
exec (0x10:remain) st locals funcs = case calling_func of
    Right val -> exec remaining (val: drop param_count st) locals funcs
    err@(Left _) -> err
    where
        (func_index, remaining) = vmParseNumber remain
        (WasmModule wf wasmFuncBodies) = funcs
        (WasmFunction param_count _) = wf !! func_index 
        (bytes, local_decl_count) = wasmFuncBodies !! func_index
        calling_func = exec bytes [] (take param_count st ++ replicate local_decl_count 0) funcs
exec _ _ _ _ = Left "tetet"