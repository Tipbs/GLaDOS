module Lib (readExpr, eval, trapError, Env, throwError) where
import Parser (lispValP, Parser (runParser), LispVal (..), ThrowsError, LispError (..))
import Control.Monad.Except (MonadError(catchError, throwError), ExceptT)
import Data.IORef (IORef, newIORef)
import Data.Maybe (fromMaybe)

type Env = [(String, LispVal)]

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

-- https://people.csail.mit.edu/jaffer/r5rs/Pairs-and-lists.html
car :: [LispVal] -> ThrowsError LispVal
car [List (f: _)] = return f
car [DottedList (f: _) _] = return f
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArg = throwError $ NumArgs 1 badArg

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_: xs)] = return $ List xs
cdr [DottedList [_] d] = return d
cdr [DottedList (_: xs) d] = return $ DottedList xs d
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArg = throwError $ NumArgs 1 badArg

cons :: [LispVal] -> ThrowsError LispVal
cons [f, List l] = return $ List $ f : l
cons [f, DottedList l la] = return $ DottedList (f: l) la
cons [x1, x2] = return $ DottedList [x1] x2
cons badArg = throwError $ NumArgs 2 badArg

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool val, Bool val2] = return $ Bool $ val == val2
eqv [Number val, Number val2] = return $ Bool $ val == val2
eqv [String val, String val2] = return $ Bool $ val == val2
eqv [Atom val, Atom val2] = return $ Bool $ val == val2
eqv [DottedList xs x, DottedList ys y] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [List x, List y] = return $ Bool $ length x == length y && all eqvPair (zip x y)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left _ -> False -- cas impossible
                                Right (Bool val) -> val
                                Right _ -> False -- cas impossible
eqv [_, _] = return $ Bool False
eqv badArg = throwError $ NumArgs 2 badArg

-- appelle juste fonction sur tout éléments, accumule résultats avec fold
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                                left <- unpacker $ head args
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv)
            ]

-- apply :: LispVal -> [LispVal] -> ThrowsError LispVal
-- apply temp@(Func params body) args = return temp
-- apply (LangFunc func) args = func args
--                         -- ($ args)
--                         -- (lookup func primitives)

apply :: String -> [(LispVal, Env)] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ lispList)
                        (lookup func primitives)
    where
        lispList = map fst args

liftEnv :: Env -> ThrowsError LispVal -> ThrowsError (LispVal, Env) 
liftEnv _ (Left err) = throwError err
liftEnv env (Right val) = return (val, env)

defineVar :: String -> (LispVal, Env) -> ThrowsError (LispVal, Env)
defineVar var (form, env) = case lookup var env of
    Just _ -> throwError $ AlreadyDefinedVar $ "Variable already defined: " ++ var
    Nothing -> return (form, (var, form) : env)

removeItem :: Env -> String -> Env
removeItem [] _ = []
removeItem ((x,y):xs) var
    | x == var = xs
    | otherwise = (x,y): removeItem xs var

setVar :: String -> (LispVal, Env) -> ThrowsError (LispVal, Env)
setVar var (form, env) = case lookup var env of
    Nothing -> throwError $ UnboundVar "Unbound variable" var
    Just _ -> return (form, (var, form) : removeItem env var)

eval :: Env -> LispVal -> ThrowsError (LispVal, Env)
eval env val@(String _) = return (val, env) -- @ totalement op, permet de récupérer toute la variable malgré pattern matching
eval env val@(Number _) = return (val, env)
eval env val@(Bool _) = return (val, env)
eval env (Atom varname) = parsed env (lookup varname env)
    where
        parsed :: Env -> Maybe LispVal -> ThrowsError (LispVal, Env)
        parsed _ Nothing = throwError $ UnboundVar "Unbound variable" varname
        parsed e (Just var) = return (var, e)
eval env (List [Atom "quote", val]) = return (val, env)
eval env (List [Atom "if", cond, itrue, ifalse]) =
     do result <- eval env cond
        case result of
             (Bool False, _) -> eval env ifalse
             _  -> eval env itrue
eval env (List [Atom "set", Atom var, form]) = eval env form >>= setVar var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftEnv env . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readExpr :: String -> ThrowsError LispVal
readExpr input = case runParser lispValP input of
   Just (val, "") -> return val
   _ -> throwError $ ParserErr "Error while parsing"
