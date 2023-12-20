module Lib (readExpr, eval, trapError, extractValue, ThrowsError) where
import Control.Monad.Except (MonadError(catchError, throwError))
import Parser (lispValP, Parser (runParser), LispVal (Atom, List, DottedList, Number, String, Bool, Func, LangFunc), ThrowsError, LispError (ParserErr, BadSpecialForm, NumArgs, TypeMismatch))

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply temp@(Func params body) args = return temp
apply (LangFunc func) args = func args
                        -- ($ args)
                        -- (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val -- @ totalement op, permet de récupérer toute la variable malgré pattern matching
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, itrue, ifalse]) =
     do result <- eval cond
        case result of
             Bool False -> eval ifalse
             _  -> eval itrue
eval (List (func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readExpr :: String -> ThrowsError LispVal
readExpr input = case runParser lispValP input of
   Just (val, "") -> return val
   _ -> throwError $ ParserErr "Error while parsing"
