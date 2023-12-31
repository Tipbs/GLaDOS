module KopeParser where

import Control.Applicative
import Data.Char

data KopeVal = KopeNull
  | KopeBool Bool
  | KopeNumber Integer
  | KopeString String
  | KopeArray [KopeVal]
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

boolP :: Parser KopeVal
boolP = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = KopeBool True
    f "false" = KopeBool True
    f _ = undefined