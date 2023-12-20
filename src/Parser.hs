{-# LANGUAGE InstanceSigs #-}
module Parser (LispVal (..), lispValP, charP, stringP, Parser (runParser),
    LispError (..), ThrowsError) where
import Control.Applicative (Alternative (empty, some, many), (<|>))
import Data.Char (isDigit, isLetter, isSpace)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | LangFunc ([LispVal] -> ThrowsError LispVal)
             | Func [String] [LispVal] -- args / body
            --  deriving (Eq)
instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | ParserErr String
                | AlreadyDefinedVar String
instance Show LispError where show = showError

type ThrowsError = Either LispError -- on donne que la première partie du Either, le reste doit-être passé au type

type Env = [(String, LispVal)]

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (AlreadyDefinedVar message)  = message
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (ParserErr parseErr)             = "Parse error at " ++ show parseErr

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

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap fun (Parser p) = Parser f
        where
            f input = do
                (x, xs) <- p input
                Just (fun x, xs)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \input -> Just (x, input)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b -- voir exemples avec Maybe
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (f, inp) <- p1 input
        (second, inp') <- p2 inp
        Just (f second, inp')

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = Parser f
    where
        f (x : xs)
            | c == x = Just (c, xs)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

letterP :: Parser Char
letterP = Parser f
    where
        f (x: xs) | isLetter x = Just (x, xs)
        f _ = Nothing

-- manyP :: Parser [a]
-- manyP = traverse

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just $ span f input

notNull :: Parser [a] -> Parser [a]
notNull p = Parser $ \input -> do
            (x, inp) <- runParser p input
            if null x
                then Nothing
                else Just (x, inp)

lispNumberP :: Parser LispVal
lispNumberP = f <$> notNull (spanP isDigit)
    where
        f d = Number $ read d

stringLiteral :: Parser String
stringLiteral = spanP (/= '\"')

lispStringP :: Parser LispVal
lispStringP = String <$> (charP '\"' *> stringLiteral <* charP '\"')

oneOfP :: [Char] -> Parser Char
oneOfP ch = Parser f
    where
        f (x: xs) | x `elem` ch = Just (x, xs)
        f _ = Nothing

symbolP :: Parser Char
symbolP = oneOfP "!#$%&|*+-/:<=>?@^_~"

lispAtomP :: Parser LispVal
lispAtomP = f <$> validAtom
    where
        validAtom = some (symbolP <|> letterP)
        f a = case a of
            "#t" -> Bool True
            "#f" -> Bool False
            _ -> Atom a

skipManyP :: (Char -> Bool) -> Parser [Char]
skipManyP = spanP

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP sep element = many (sep *> element <* sep)

lispListP :: Parser LispVal
lispListP = List <$> (charP '(' *> elements <* charP ')')
    where
        elements = sepByP sep lispValP
        sep = skipManyP isSpace

lispQuotedP :: Parser LispVal
lispQuotedP = f <$> (charP '\'' *> lispValP)
    where
        f a = List [Atom "quote", a]

lispValP :: Parser LispVal
lispValP = lispNumberP <|> lispStringP <|> lispAtomP <|> lispListP <|> lispQuotedP

-- atomP :: Parser LispVal
-- atomP str = fmap transf $ letterP str *> manyP
--     where
--         transf "#t" = Bool True
--         transf "#f" = Bool False
--         transf atom = Atom atom
