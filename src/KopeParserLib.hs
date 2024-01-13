module KopeParserLib where

import Control.Applicative
import Data.Char

data KopeVal = KopeNull
  | KopeAtom String
  | KopeBool Bool
  | KopeNumber Integer
  | KopeString String
  | KopeArray [KopeVal]
  | KopeFunc { funcName :: String, funcParams :: [String],
               funcBody :: [KopeVal] }
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

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', x) <- p input
  case null x of
    True -> Nothing
    False -> Just (input', x)

-- (a -> Bool) -> [a] -> Parser (String, [a])
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> do
  let (valid, rest) = span f input
  Just (rest, valid)

ws :: Parser String
ws = spanP isSpace <|> spanP (`elem` "\n")

oneOfP :: [String] -> Parser KopeVal
oneOfP tokens = Parser $ \input -> f tokens input
    where
        f (x: xs) input
          | runParser (stringP x) input == Nothing = f xs input
          | otherwise = runParser (KopeAtom <$> stringP x) input
        f _ _ = Nothing

charP :: Char -> Parser Char
charP a = Parser $ \input ->
  case input of
    (x:xs) | x == a -> Just (xs, a)
    _ -> Nothing

-- [Char] -> [Parser Char] -> Parser [Char]
stringP :: String -> Parser String
stringP = sequenceA . map charP

letterP :: Parser String
letterP = notNull $ spanP (\input -> isLetter input || (== '_') input)

-- removeNewline :: String -> String
-- removeNewline xs = [ x | x <- xs, x /= '\n' ]
