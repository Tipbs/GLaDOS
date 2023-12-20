module Lib (readExpr, eval, trapError, extractValue, Env) where
-- import Text.ParserCombinators.Parsec
--     ( char,
--       digit,
--       letter,
--       noneOf,
--       oneOf,
--       space,
--       endBy,
--       many1,
--       sepBy,
--       skipMany1,
--       (<|>),
--       many,
--       parse,
--       try,
--       ParseError,
--       Parser )
import Control.Monad.Except (MonadError(catchError, throwError), ExceptT)
import Parser (lispValP, Parser (runParser), LispVal (Atom, List, DottedList, Number, String, Bool))
import Data.IORef (IORef, newIORef)
import Data.Maybe (fromMaybe)

-- data LispVal = Atom String
--              | List [LispVal]
--              | DottedList [LispVal] LispVal
--              | Number Integer
--              | String String
--              | Bool Bool
-- instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | BadSpecialForm String LispVal
                | Parser String
                | NotFunction String String
                | UnboundVar String String
                | AlreadyDefinedVar String
instance Show LispError where show = showError

type ThrowsError = Either LispError -- on donne que la première partie du Either, le reste doit-être passé au type

type Env = [(String, LispVal)]

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (AlreadyDefinedVar message)  = message
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

-- parseDottedList :: Parser LispVal
-- parseDottedList = do
--     h <- endBy parseExpr spaces
--     t <- char '.' >> spaces >> parseExpr
--     return $ DottedList h t

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

-- comprendre unpackNum
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

apply :: String -> [(LispVal, Env)] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ lispList)
                        (lookup func primitives)
    where
        lispList = map fst args

liftEnv :: Env -> ThrowsError LispVal -> ThrowsError (LispVal, Env) 
liftEnv env (Left err) = throwError err
liftEnv env (Right val) = return (val, env)

defineVar :: String -> (LispVal, Env) -> ThrowsError (LispVal, Env)
defineVar var (form, env) = case lookup var env of
    Just alreadyExist -> throwError $ AlreadyDefinedVar $ "Variable already defined: " ++ var
    Nothing -> return (form, (var, form) : env)

eval :: Env -> LispVal -> ThrowsError (LispVal, Env)
eval env val@(String _) = return (val, env) -- @ totalement op, permet de récupérer toute la variable malgré pattern matching
eval env val@(Number _) = return (val, env)
eval env val@(Bool _) = return (val, env)
eval env (Atom varname) = parsed env (lookup varname env)
    where
        parsed :: Env -> Maybe LispVal -> ThrowsError (LispVal, Env)
        parsed e Nothing = throwError $ UnboundVar "Unbound variable" varname
        parsed e (Just var) = return (var, e)

eval env (List [Atom "quote", val]) = return (val, env)
eval env (List [Atom "if", cond, itrue, ifalse]) =
     do result <- eval env cond
        case result of
             (Bool False, _) -> eval env ifalse
             _  -> eval env itrue
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftEnv env . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readExpr :: String -> ThrowsError LispVal
readExpr input = case runParser lispValP input of
   Just (val, "") -> return val
   _ -> throwError $ Parser "Error while parsing"
