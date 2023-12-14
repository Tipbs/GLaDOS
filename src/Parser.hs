{-# LANGUAGE InstanceSigs #-}
module Parser (LispVal, lispValP, charP, stringP, Parser (runParser)) where
import Control.Applicative (Alternative (empty, some, many), (<|>))
import Data.Char (isDigit, isLetter, isSpace)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show, Eq)

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
    empty = Parser $ \_ -> Nothing
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
sepByP sep element = many (element <* sep)

lispListP :: Parser LispVal
lispListP = List <$> (charP '(' *> elements <* charP ')')
    where
        elements = sepByP sep lispValP
        sep = skipManyP isSpace

lispValP :: Parser LispVal
lispValP = lispNumberP <|> lispStringP <|> lispAtomP <|> lispListP

-- atomP :: Parser LispVal
-- atomP str = fmap transf $ letterP str *> manyP
--     where
--         transf "#t" = Bool True
--         transf "#f" = Bool False
--         transf atom = Atom atom