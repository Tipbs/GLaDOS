module KopeParser where

import Control.Applicative
import Data.Char

data KopeVal = KopeNull
  | KopeAtom String
  | KopeBool Bool
  | KopeNumber Integer
  | KopeString String
  | KopeArray [KopeVal]
  | KopeFunc { name :: String, params :: [String],
               body :: [KopeVal] }
  deriving (Show, Eq)

-- no error reporting
newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
  fmap f (Parser a) = Parser $ \input -> do
    (input', x) <- a input
    Just (input', f x)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  (Parser f) <*> (Parser a) = Parser $ \input -> do
    (input', f') <- f input
    (input'', a') <- a input'
    Just (input'', f' a')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input
  
charP :: Char -> Parser Char
charP a = Parser $ \input ->
  case input of
    (x:xs) | x == a -> Just (xs, a)
    _ -> Nothing

-- [Char] -> [Parser Char] -> Parser [Char]
stringP :: String -> Parser String
stringP = sequenceA . map charP

kopeBool :: Parser KopeVal
kopeBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = KopeBool True
    f "false" = KopeBool True
    f _ = undefined

-- (a -> Bool) -> [a] -> Parser (String, [a])
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> do
  let (valid, rest) = span f input
  Just (rest, valid)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', x) <- p input
  case null x of
    False -> Nothing
    True -> Just (input', x)

kopeNumber :: Parser KopeVal
kopeNumber = f <$> spanP isDigit
  where
    f num = KopeNumber $ read num

ws :: Parser String
ws = spanP isSpace

stringLiteral :: Parser String
stringLiteral = spanP (/= ' ')

kopeValue :: Parser KopeVal
kopeValue = kopeBool <|> kopeNumber

varP :: Parser KopeVal
varP = KopeArray <$> pair
  where
    pair =
      (\var _ value -> [KopeAtom "assign", KopeString var, value]) <$>
      stringLiteral <*> (ws *> charP '=' <* ws) <*>
      kopeValue

lineP :: Parser KopeVal
lineP = varP

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP sep element = many (ws *> element <* ws <* sep <* ws)

bodyP :: Parser KopeVal
bodyP = KopeArray <$> (charP '{' *> ws *> declaration <* ws <* charP '}')
  where
    declaration = sepByP (charP ';') lineP

paramsP :: Parser KopeVal
paramsP = KopeArray <$> (charP '(' *> ws *> declaration <* ws <* charP ')') 
  where
    declaration = sepByP (charP ',') (KopeString <$> stringLiteral)

kopeFuncP :: Parser KopeVal 
kopeFuncP = Parser $ \input -> Just (input, KopeFunc name params body)
  where
    name = undefined
    params = undefined
    body = undefined
