module KopeParser (parseFile) where

import KopeParserLib
import Control.Applicative
import Data.Char

kopeBool :: Parser KopeVal
kopeBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = KopeBool True
    f "false" = KopeBool False
    f _ = undefined

kopeNumber :: Parser KopeVal
kopeNumber = f <$> notNull (spanP isDigit)
  where
    f num = KopeNumber $ read num

kopeVar :: Parser KopeVal
kopeVar = KopeAtom <$> notNull letterP

stringLiteral :: Parser String
stringLiteral = spanP (/= ' ')

kopeAtom :: Parser KopeVal
kopeAtom = oneOfP ["==", "/=", ">=", "<=", "&&", "||", "+", "-", "*", "/", "<", ">"]

kopeValue :: Parser KopeVal
kopeValue = kopeFunc <|> kopeLoop <|> kopeCond <|> kopeCall <|> kopeReturn <|> kopeVarSet <|> kopeTest <|>
            kopeAtom <|> kopeBool <|> kopeVar <|> kopeString <|> kopeNumber


-- atomP :: String -> Parser KopeVal
-- atomP atom = KopeArray <$> pair
--   where
--     pair =
--       (\var _ value -> [KopeAtom atom, var, value]) <$>
--       kopeValue <*> (ws *> stringP atom <* ws) <*>
--       kopeValue

setVarP :: Parser KopeVal
setVarP = KopeArray <$> pair
  where
    pair =
      (\var _ value -> [KopeAtom "set", var, value]) <$>
      kopeVar <*> (ws *> charP '=' <* ws) <*> kopeExpr

defineVarP :: Parser KopeVal
defineVarP = KopeArray <$> pair
  where
    pair =
      (\var _ value -> [KopeAtom "define", var, value]) <$>
      (stringP "var" *> ws *> kopeVar) <*>
      (ws *> charP '=' <* ws) <*> kopeExpr

kopeVarSet :: Parser KopeVal
kopeVarSet = setVarP <|> defineVarP

-- kopeLine :: Parser KopeVal
-- kopeLine =
--         atomP "==" <|>
--         atomP "+" <|>
--         atomP "-" <|>
--         atomP "*" <|>
--         atomP "/" <|>
--         atomP "=" <|>
--         atomP "<" <|>
--         atomP ">" <|>
--         atomP "/=" <|>
--         atomP ">=" <|>
--         atomP "<=" <|>
--         atomP "&&" <|>
--         atomP "||"

-- kopeCond :: Parser kopeVal
-- kopeCond = KopeArray <$> 

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP sep element = (:) <$> (ws *> element) <*> many (ws *> sep *> ws *> element <* ws) <|> pure []

sepByEndP :: Parser a -> Parser b -> Parser [b]
sepByEndP sep element = many (ws *> element <* ws <* sep <* ws)

bodyP :: Parser [KopeVal]
bodyP = charP '{' *> ws *> declaration <* ws <* charP '}'
  where
    declaration = sepByEndP (charP ';') kopeExpr

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

kopeReturn :: Parser KopeVal
kopeReturn = KopeArray <$> pair
  where
    pair =
      (\value -> [KopeAtom "return", value]) <$>
      (stringP "return" *> ws *> kopeExpr <* ws)

kopeString :: Parser KopeVal
kopeString = KopeString <$> (charP '"' *> spanP (/= '"') <* charP '"')

paramsCallP :: Parser [KopeVal]
paramsCallP = ws *> charP '(' *> declaration <* charP ')' <* ws
  where
    declaration = sepByP (charP ',') kopeExpr

nameCallP :: Parser KopeVal
nameCallP = KopeAtom <$> letterP

kopeCall :: Parser KopeVal
kopeCall = KopeArray  <$> ((:) <$> nameCallP <*> paramsCallP)
-- kopeCall = KopeArray . map KopeString <$> ((:) <$> letterP <*> paramsP)
-- kopeCall = (:) <$> (KopeArray . map KopeString <$> paramsP)

kopeTest :: Parser KopeVal
kopeTest = charP '(' *> ws *> kopeExpr <* ws <* charP ')'

kopeCond :: Parser KopeVal
kopeCond = KopeArray <$> pair
  where
    pair =
      (\cond body -> [KopeAtom "if", cond, KopeArray body]) <$>
      (stringP "if" *> ws *> kopeTest <* ws) <*>
      bodyP

kopeLoop :: Parser KopeVal
kopeLoop = KopeArray <$> pair
  where
    pair =
      (\cond body -> [KopeAtom "while", cond, KopeArray body]) <$>
      (stringP "while" *> ws *> kopeTest <* ws) <*>
      bodyP

parseExpr :: [[String]] -> Parser KopeVal
parseExpr [] = kopeValue
parseExpr (x: xs) = Parser $ \input -> do
      (ts1, f1) <- runParser (parseExpr xs) input
      go f1 ts1
  where
    go acc "" = Just ("", acc)
    go acc rest | runParser ((ws *> kopeValue <* ws) *> (parseExpr xs)) rest == Nothing = Just (rest, acc) -- to check if Monad will not fail (Nothin)
    go acc rest = do
        (ts2, f1) <- runParser (ws *> kopeValue <* ws) rest
        (ts3, f2) <- runParser (parseExpr xs) ts2
        case f1 of
          KopeAtom atom | atom `elem` x -> go (KopeArray [f1, acc, f2]) ts3
          _ -> Just (rest, acc)

kopeExpr :: Parser KopeVal
kopeExpr = parseExpr [["&&", "||"], ["==", "/=", "<=", ">=", "<", ">"], ["+", "-"], ["*", "/"]]

kopeFile :: Parser KopeVal
kopeFile = KopeArray <$> many (ws *> kopeFunc <* ws)

parseFile :: FilePath -> IO (Maybe KopeVal)
parseFile file = do
  input <- readFile file
  return (snd <$> runParser kopeFile input)
