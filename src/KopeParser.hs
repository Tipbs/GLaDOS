module KopeParser where

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
kopeVar = KopeAtom <$> letterP

stringLiteral :: Parser String
stringLiteral = spanP (/= ' ')

kopeValue :: Parser KopeVal
kopeValue = kopeNumber <|> kopeBool <|> kopeString <|> kopeVar <|> kopeCall

kopeExpr :: Parser KopeVal
kopeExpr = kopeFunc <|> kopeCond <|> kopeLoop <|> kopeLine

atomP :: String -> Parser KopeVal
atomP atom = KopeArray <$> pair
  where
    pair =
      (\var _ value -> [KopeAtom atom, var, value]) <$>
      kopeValue <*> (ws *> stringP atom <* ws) <*>
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
kopeLine = 
        atomP "==" <|>
        atomP "+" <|>
        atomP "-" <|>
        atomP "*" <|>
        atomP "/" <|>
        atomP "=" <|>
        atomP "<" <|>
        atomP ">" <|>
        atomP "/=" <|>
        atomP ">=" <|>
        atomP "<=" <|>
        atomP "&&" <|>
        atomP "||"

-- kopeCond :: Parser kopeVal
-- kopeCond = KopeArray <$> 

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

paramsCallP :: Parser [KopeVal]
paramsCallP = ws *> charP '(' *> declaration <* charP ')' <* ws
  where
    declaration = sepByP (charP ',') kopeValue

nameCallP :: Parser KopeVal
nameCallP = KopeString <$> letterP

kopeCall :: Parser KopeVal
kopeCall = KopeArray  <$> ((:) <$> nameCallP <*> paramsCallP)
-- kopeCall = KopeArray . map KopeString <$> ((:) <$> letterP <*> paramsP)
-- kopeCall = (:) <$> (KopeArray . map KopeString <$> paramsP)

kopeTest :: Parser KopeVal
kopeTest = charP '(' *> ws *> kopeValue <* ws <* charP ')'

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

-- kopeComp :: Parser char
-- kopeComp = oneof ""
