module Parser (parseChar, parseAnyChar, parseOr, parseAnd, parseAndWith, parseMany, parseSome) where
import Data.Maybe (isJust, fromJust, maybeToList)
import Data.List (unfoldr)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \input ->
        case runParser parser input of
            Nothing -> Nothing
            Just (result, remaining) -> Just (fct result, remaining)

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
  case input of
    (x:xs) | x == c -> Just (x, xs)
    _ -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar chars = Parser $ \input ->
    case input of
        (x:xs) | x `elem` chars -> Just (x, xs)
               | otherwise -> Nothing
        _ -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser $ \str ->
    case runParser p1 str of
        Just result -> Just result
        Nothing -> runParser p2 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \str ->
    case runParser p1 str of
        Nothing -> Nothing
        Just (parsed, xs) -> case runParser p2 xs of
            Just (parsed2, xs2) -> Just ((parsed, parsed2), xs2)
            Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fn p1 p2 = Parser $ \str -> 
    case runParser p1 str of
        Nothing -> Nothing
        Just (parsed, xs) -> case runParser p2 xs of
            Just (parsed2, xs2) -> Just (fn parsed parsed2, xs2)
            Nothing -> Nothing

{-parseMany :: Parser a -> Parser [a]
parseMany p1 = Parser $ \str ->
    Just (fir, finalStr)
    where
        -- taken = takeWhile (/= Nothing) $ iterate p1 (p1 str)
        rec s = case runParser p1 s of
            Just (parsed, xs) -> (parsed, xs) : rec xs
            Nothing -> []
        recCalculated = rec str
        fir = map fst recCalculated
        finalStr = if null recCalculated then str else snd $ last recCalculated-}

parseMany :: Parser a -> Parser [a]
parseMany parser = Parser $ \input ->
    case runParser parser input of
    Nothing -> Just ([], input)
    Just (result, rest) -> do
        (results, remaining) <- runParser (parseMany parser) rest
        Just (result : results, remaining)

{-parseSome :: Parser a -> Parser [a]
parseSome p1 str
    | null recCalculated = Nothing
    | otherwise = Just (fir, finalStr)
    where
        -- taken = takeWhile (/= Nothing) $ iterate p1 (p1 str)
        rec s = case p1 s of
            Just (parsed, xs) -> (parsed, xs) : rec xs
            Nothing -> []
        recCalculated = rec str
        fir = map fst recCalculated
        finalStr = if null recCalculated then str else snd $ last recCalculated-}

parseSome :: Parser a -> Parser [a]
parseSome parser = Parser $ \str ->
    case runParser (parseMany parser) str of
        Just ([], _) -> Nothing
        Just (result, remaining) -> Just (result, remaining)

parseUInt :: Parser Int
parseUInt = Parser $ \str ->
    case runParser many_result str of
        Just ([], _) -> Nothing
        Just (result , remaining) -> Just (read result::Int, remaining)
    where
        many_result = parseMany (parseAnyChar ['0'..'9'])

parseInt :: Parser Int
parseInt = Parser $ \str ->
    case str of
        [] -> Nothing
        _ ->
            let (sign, rest) = if head str == '-' then ("-", tail str) else ("", str)
                many_result = runParser (parseMany (parseAnyChar ['0'..'9'])) rest
                in case many_result of
                    Just ([], _) -> Nothing
                    Just (result , remaining) -> Just (read (sign ++ result)::Int, remaining)  

parsePair :: Parser a -> Parser (a, a)
parsePair parser = Parser $ \input ->
    let (isPar, rest) = if head input == '(' then (True, tail input) else (False, "")
    in case isPar of
        True -> do
            (result, remaining) <- runParser parser rest
            (_, remChar1) <- runParser (parseChar ' ') remaining
            (result2, remaining2) <- runParser parser remChar1
            (_, remaining_str) <- runParser (parseChar ')') remaining2
            return ((result, result2), remaining_str)
        False -> Nothing

sepBy :: String -> Char -> String
sepBy [] _ = []
sepBy (x:xs) c
    | x == c = xs
    | otherwise = x:xs

parseListRec :: Parser a -> Parser [a]
parseListRec parser = Parser $ \input ->
    case runParser parser input of
        Nothing -> Just ([], input)
        Just (result, rest) -> do
            case sepBy rest ' ' of
                [] -> do 
                    (results, remaining) <- runParser (parseListRec parser) rest
                    Just (result : results, remaining)
                rem -> do
                    (results, remaining) <- runParser (parseListRec parser) rem
                    Just (result : results, remaining)

parseList :: Parser a -> Parser [a]
parseList parser = Parser $ \input ->
    let (isPar, rest) = if head input == '(' then (True, tail input) else (False, "")
    in case isPar of
        True -> case runParser (parseListRec parser) (sepBy rest ' ') of
            Just (result, ')':xs) -> Just (result, xs)
            Just (result, remaining) -> Just (result, remaining)
        False -> Nothing

