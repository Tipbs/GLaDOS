module Bytecoder (compileToBytecode) where
import Data.Maybe (isJust, fromJust, maybeToList)
import Data.List (unfoldr)
import Control.Applicative
import Lib (LispVal(Atom, List, DottedList, Number, String, Bool))
import Data.Bits
import Data.Int

pushBool :: Bool -> [Bool]
pushBool True = [False, False, False, False]
pushBool _ = [False, False, False, True]

numToBoolArr :: Integer -> Int -> [Bool]
numToBoolArr _ 64 = []
numToBoolArr num count = numToBoolArr num (count+1) ++ [num `testBit` count]

pushNumber :: Integer -> [Bool]
pushNumber num = [True, False] ++ numToBoolArr num 0

compileToBytecode :: LispVal -> [Bool]
-- compileToBytecode (Atom ato) = pushAtom ato
-- compileToBytecode (List exprs) = concatMap compileToBytecode exprs
-- compileToBytecode (DottedList exprs last) = concatMap compileToBytecode (exprs ++ '.' ++ last)
compileToBytecode (Number num) = pushNumber num
-- compileToBytecode (String str) = pushString str
compileToBytecode (Bool bol) = pushBool bol

showBytesGlobal :: [Bool] -> String
showBytesGlobal li = "[" ++ showBytes li ++ " ]"

showBytes :: [Bool] -> String
showBytes [] = ""
showBytes (a:b:r) = " " ++ showBit a ++ showBit b ++ showBytes r

showBit :: Bool -> [Char]
showBit True = "1"
showBit False = "0"

test :: LispVal -> String
test l = showBytesGlobal( compileToBytecode l )