module Bytecoder (compileToBytecode, showBytesGlobal) where
import Lib (LispVal(Atom, List, Number, String, Bool, DottedList))
import Data.Bits
import Data.Int
import Data.Char
import Prelude

pushBool :: Bool -> [Bool]
pushBool True = [False, False, False, False]
pushBool _ = [False, False, False, True]

numToBoolArr :: Integer -> Int -> Int -> [Bool]
numToBoolArr _ a b | a == b = []
numToBoolArr num count maxi = numToBoolArr num (count+1) maxi ++ [num `testBit` count]

numToBoolArr2 :: Int -> Int -> Int -> [Bool]
numToBoolArr2 _ a b | a == b = []
numToBoolArr2 num count maxi = numToBoolArr2 num (count+1) maxi ++ [num `testBit` count]

pushNumber :: Integer -> [Bool]
pushNumber num = [True, False] ++ numToBoolArr num 0 64

getStrLen :: String -> Integer
getStrLen [] = 0
getStrLen (_:b) = 1 + getStrLen b
strToBoolArr :: String -> [Bool]
strToBoolArr = foldr (\ a -> (++) (numToBoolArr2 (ord a) 0 8)) []

pushString :: String -> [Bool]
pushString str = [False, True] ++ numToBoolArr (getStrLen str) 0 12 ++ strToBoolArr str  -- 8 = Max 256 char. 12 = Max 4096 Char. 16 = Max 65536 Char (Unicode may count more than 1)

pushAtom :: String -> [Bool]
pushAtom a = [True, True] ++ numToBoolArr (getStrLen a) 0 4 ++ strToBoolArr a

compileToBytecode :: LispVal -> [Bool]
compileToBytecode (Atom ato) = pushAtom ato
compileToBytecode (List exprs) = numToBoolArr2 (ord '[') 0 8 ++ concatMap compileToBytecode exprs ++ numToBoolArr2 (ord ']') 0 8
compileToBytecode (DottedList exprs lasto) = numToBoolArr2 (ord '{') 0 8 ++ concatMap compileToBytecode exprs 
    ++ numToBoolArr2 (ord '.') 0 8 ++ compileToBytecode lasto ++ numToBoolArr2 (ord '}') 0 8
compileToBytecode (Number num) = pushNumber num
compileToBytecode (String str) = pushString str
compileToBytecode (Bool bol) = pushBool bol

showBytesGlobal :: [Bool] -> String
showBytesGlobal li = "[" ++ showBytes li ++ " ]"

showBytes :: [Bool] -> String
showBytes [] = ""
showBytes (a:b:r) = " " ++ showBit a ++ showBit b ++ showBytes r
showBytes _ = []

showBit :: Bool -> [Char]
showBit True = "1"
showBit False = "0"

test :: LispVal -> String
test l = showBytesGlobal ( compileToBytecode l )