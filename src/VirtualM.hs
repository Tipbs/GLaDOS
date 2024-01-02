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

addi :: Stack -> Stack
addi [] = []
addi [a] = [a]
addi (Numerical a:Numerical b:c) = Numerical (a + b):c
addi st = st

subs :: Stack -> Stack
subs [] = []
subs [a] = [a]
subs (Numerical a:Numerical b:c) = Numerical (a - b):c
subs st = st

mult :: Stack -> Stack
mult [] = []
mult [a] = [a]
mult (Numerical a:Numerical b:c) = Numerical (a * b):c
mult st = st

divi :: Stack -> Stack
divi [] = []
divi [a] = [a]
divi (Numerical a:Numerical 0:c) = Numerical a:Numerical 0:c
divi (Numerical a:Numerical b:c) = Numerical (a `div` b):c
divi st = st

call :: Builtin -> Stack -> Stack
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

exec :: Insts -> Stack -> Value
exec [] _ = Numerical 0
exec (Ret:_) st = ret st
exec ((Push val):remain) st = exec remain (push val st)
exec ((Call bi):remain) st = exec remain (call bi st)