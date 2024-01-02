{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module VirtualM () where

type Stack = [Value]
type Insts = [Instructions]

data Value = Numerical Int
           | Boolean Bool
           deriving(Eq, Show, Read)

data Builtin = Add
             | Sub
             | Mult
             | Div

data Instructions = Push Value
                  | Call Builtin
                  | Ret

addi :: Stack -> Either String Stack
addi [] = Left "Error : Not enough arguments in Add."
addi [_] = Left "Error : Not enough arguments in Add."
addi (Numerical a:Numerical b:c) = Right (Numerical (a + b):c)
addi _ = Left "Error : One argument is a wrong type in Add."

subs :: Stack -> Either String Stack
subs [] = Left "Error : Not enough arguments in Sub."
subs [_] = Left "Error : Not enough arguments in Sub."
subs (Numerical a:Numerical b:c) = Right (Numerical (a - b):c)
subs _ = Left "Error : One argument is a wrong type in Sub."

mult :: Stack -> Either String Stack
mult [] = Left "Error : Not enough arguments in Mult."
mult [_] = Left "Error : Not enough arguments in Mult."
mult (Numerical a:Numerical b:c) = Right (Numerical (a * b):c)
mult _ = Left "Error : One argument is a wrong type in Mult."

divi :: Stack -> Either String Stack
divi [] = Left "Error : Not enough arguments in Div."
divi [_] = Left "Error : Not enough arguments in Div."
divi (Numerical _:Numerical 0:_) = Left "Error : Divison by 0."
divi (Numerical a:Numerical b:c) = Right (Numerical (a `div` b):c)
divi _ = Left "Error : One argument is a wrong type in Div."

call :: Builtin -> Stack -> Either String Stack
call Add = addi
call Sub = subs
call Mult = mult
call Div = divi

push :: Value -> Stack -> Stack
push val st = val:st

ret :: Stack -> Value
ret [] = Numerical 0
ret [a] = a
ret (a:_) = a

exec :: Insts -> Stack -> Either String Value
exec [] _ = Left "Error : No return instruction."
exec (Ret:_) st = Right (ret st)
exec ((Push val):remain) st = exec remain (push val st)
exec ((Call bi):remain) st = do let res = call bi st
                                case res of
                                    Right stac -> exec remain stac
                                    Left str -> Left str