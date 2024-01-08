module KopeParser where

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
    True -> Nothing
    False -> Just (input', x)

kopeNumber :: Parser KopeVal
kopeNumber = f <$> spanP isDigit
  where
    f num = KopeNumber $ read num

ws :: Parser String
ws = spanP isSpace

letterP :: Parser String
letterP = spanP (\input -> isLetter input || (== '_') input)

kopeVar :: Parser KopeVal
kopeVar = KopeAtom <$> letterP

stringLiteral :: Parser String
stringLiteral = spanP (/= ' ')

kopeValue :: Parser KopeVal
kopeValue = kopeBool <|> kopeString

kopeExpr :: Parser KopeVal
kopeExpr = kopeFunc <|> kopeLine

atomP :: String -> Parser KopeVal
atomP atom = KopeArray <$> pair
  where
    pair =
      (\var _ value -> [KopeAtom atom, KopeString var, value]) <$>
      stringLiteral <*> (ws *> stringP atom <* ws) <*>
      kopeValue

setVarP :: Parser KopeVal
setVarP = KopeArray <$> pair
  where
    pair =
      (\var _ value -> [KopeAtom "set", KopeString var, value]) <$>
      stringLiteral <*> (ws *> charP '=' <* ws) <*> kopeValue

defineVarP :: Parser KopeVal
defineVarP = KopeArray <$> pair
  where
    pair =
      (\var _ value -> [KopeAtom "define", KopeString var, value]) <$>
      (stringP "var" *> ws *> stringLiteral) <*>
      (ws *> charP '=' <* ws) <*> kopeValue

kopeLine :: Parser KopeVal
kopeLine = defineVarP <|>
        setVarP <|>
        atomP "==" <|>
        atomP "+" <|>
        atomP "-" <|>
        atomP "*" <|>
        atomP "/" <|>
        atomP "mod" <|>
        atomP "quotient" <|>
        atomP "remainder" <|>
        atomP "=" <|>
        atomP "<" <|>
        atomP ">" <|>
        atomP "/=" <|>
        atomP ">=" <|>
        atomP "<=" <|>
        atomP "&&" <|>
        atomP "||" <|>
        atomP "string=?" <|>
        atomP "string<?" <|>
        atomP "string>?" <|>
        atomP "string<=?" <|>
        atomP "string>=?" <|>
        atomP "car" <|>
        atomP "cdr" <|>
        atomP "cons" <|>
        atomP "eq?" <|>
        atomP "eqv?"

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP sep element = (:) <$> (ws *> element) <*> many (ws *> sep *> ws *> element <* ws) <|> pure []

sepByEndP :: Parser a -> Parser b -> Parser [b]
sepByEndP sep element = many (ws *> element <* ws <* sep <* ws)

bodyP :: Parser [KopeVal]
bodyP = charP '{' *> ws *> declaration <* ws <* charP '}'
  where
    declaration = sepByEndP (charP ';') kopeLine

paramsP :: Parser [String]
paramsP = ws *> charP '(' *> declaration <* charP ')' <* ws
  where
    declaration = sepByP (charP ',') letterP

nameP :: Parser String
nameP = stringP "fn" *> ws *> notNull letterP <* ws

kopeFunc :: Parser KopeVal 
kopeFunc = Parser $ \input -> do
  (input', name) <- nameF input
  (input'', params) <- paramsF input'
  (input''', body) <- bodyF input''
  Just (input''', KopeFunc name params body)
  where
    Parser nameF = nameP
    Parser paramsF = paramsP
    Parser bodyF = bodyP

kopeString :: Parser KopeVal
kopeString = KopeString <$> (charP '"' *> spanP (/= '"') <* charP '"')
