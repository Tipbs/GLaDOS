{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module VirtualM () where

type Stack = [Value]
type Insts = [Instructions]
type Args = [Value]

data Value = Numerical Int
           | Boolean Bool
           deriving(Eq, Show, Read)

data Builtin = Add
             | Sub
             | Mult
             | Div
             | Eq
             | Less
             | High

data Instructions = Push Value
                  | PushArg Int
                  | Call Builtin
                  | JumpIfFalse Int
                  | JumpIfTrue Int
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

eq :: Stack -> Either String Stack
eq [] = Left "Error : Not enough arguments in Eq."
eq [_] = Left "Error : Not enough arguments in Eq."
eq (Numerical a:Numerical b:c) = Right (Boolean (a == b):c)
eq _ = Left "Error : One argument is a wrong type in Eq."

less :: Stack -> Either String Stack
less [] = Left "Error : Not enough arguments in Less."
less [_] = Left "Error : Not enough arguments in Less."
less (Numerical a:Numerical b:c) = Right (Boolean (a < b):c)
less _ = Left "Error : One argument is a wrong type in Less."

high :: Stack -> Either String Stack
high [] = Left "Error : Not enough arguments in High."
high [_] = Left "Error : Not enough arguments in High."
high (Numerical a:Numerical b:c) = Right (Boolean (a > b):c)
high _ = Left "Error : One argument is a wrong type in High."

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
call Eq = eq
call Less = less
call High = high

jumptrue :: Insts -> Int -> Stack -> Either String Insts
jumptrue _ _ [] = Left "Error : Not enough argument to jump."
jumptrue [] _ _ = Left "Error : Not enough instructions to jump."
jumptrue _ _ ((Numerical _):_) = Left "Error : Argument is not a boolean."
jumptrue rema _ ((Boolean False):_) = Right rema
jumptrue (_:remai) 0 ((Boolean True):_) = Right remai
jumptrue (_:remai) num st@((Boolean True):_) = jumptrue remai (num - 1) st

jumpfalse :: Insts -> Int -> Stack -> Either String Insts
jumpfalse _ _ [] = Left "Error : Not enough argument to jump."
jumpfalse [] _ _ = Left "Error : Not enough instructions to jump."
jumpfalse _ _ ((Numerical _):_) = Left "Error : Argument is not a boolean."
jumpfalse (_:rema) _ ((Boolean True):_) = Right rema
jumpfalse (_:remai) 0 ((Boolean False):_) = Right remai
jumpfalse (_:remai) num st@((Boolean False):_) = jumpfalse remai (num - 1) st

push :: Value -> Stack -> Stack
push val st = val:st

pusharg :: Args -> Int -> Stack -> Either String Stack
pusharg [] _ _ = Left "Error : Not enough arguments."
pusharg (a:_) 0 st = Right (a:st)
pusharg (_:b) num st = pusharg b (num - 1) st

ret :: Stack -> Value
ret [] = Numerical 0
ret [a] = a
ret (a:_) = a

exec :: Args -> Insts -> Stack -> Either String Value
exec _ [] _ = Left "Error : No return instruction."
exec _ (Ret:_) st = Right (ret st)
exec arg ((Push val):remain) st = exec arg remain (push val st)
exec arg ((PushArg int):remain) st = do {
                                    let res = pusharg arg int st in
                                        case res of
                                            Right inst -> exec arg remain inst
                                            Left str -> Left str
                                            }
exec arg li@((JumpIfFalse int):_) st = do {
                                    let res = jumpfalse li int st in
                                        case res of
                                            Right inst -> exec arg inst st
                                            Left str -> Left str
                                            }
exec arg li@((JumpIfTrue int):_) st = do {
                                    let res = jumptrue li int st in
                                        case res of
                                            Right inst -> exec arg inst st
                                            Left str -> Left str
                                            }
exec arg ((Call bi):remain) st = do {
                                let res = call bi st in
                                    case res of
                                        Right stac -> exec arg remain stac
                                        Left str -> Left str
                                        }

