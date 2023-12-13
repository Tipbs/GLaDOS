module Parser (parseChar, parseAnyChar, parseOr, parseAnd, parseAndWith, parseMany, parseSome) where
import Data.Maybe (isJust, fromJust, maybeToList)
import Data.List (unfoldr)

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar _ "" = Nothing
parseChar ch (x: xs)
    | ch == x = Just (ch, xs)
    | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar _ "" = Nothing
parseAnyChar chars (x: xs)
    | x `elem` chars = Just (x, xs)
    | otherwise = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 str
    | isJust evaluated = evaluated
    | otherwise = p2 str
    where
        evaluated = p1 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 str = case evaluated of
        Nothing -> Nothing
        Just (parsed, xs) -> case p2 xs of
            Just (parsed2, xs2) -> Just ((parsed, parsed2), xs2)
            Nothing -> Nothing
    where
        evaluated = p1 str

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fn p1 p2 str = case evaluated of
        Nothing -> Nothing
        Just (parsed, xs) -> case p2 xs of
            Just (parsed2, xs2) -> Just (fn parsed parsed2, xs2)
            Nothing -> Nothing
    where
        evaluated = p1 str

parseMany :: Parser a -> Parser [a]
parseMany p1 str = Just (fir, finalStr)
    where
        -- taken = takeWhile (/= Nothing) $ iterate p1 (p1 str)
        rec s = case p1 s of
            Just (parsed, xs) -> (parsed, xs) : rec xs
            Nothing -> []
        recCalculated = rec str
        fir = map fst recCalculated
        finalStr = if null recCalculated then str else snd $ last recCalculated

parseSome :: Parser a -> Parser [a]
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
        finalStr = if null recCalculated then str else snd $ last recCalculated

parseUInt :: Parser Int
parseUInt str = case many_result of
    Just ([], _) -> Nothing
    Just (result , remaining) -> Just (read result::Int, remaining)
    where
        many_result = parseMany (parseAnyChar ['0'..'9']) str

parseInt :: Parser Int
parseInt [] = Nothing
parseInt str
    | isNeg == True = case many_result of
        Just ([], _) -> Nothing
        Just (result , remaining) -> Just (read ('-':result)::Int, remaining)  
    | otherwise = case many_result of
        Just ([], _) -> Nothing
        Just (result , remaining) -> Just (read result::Int, remaining)
    where
        isNeg = isJust $ parseChar '-' str 
        rest = if not isNeg then str else tail str
        many_result = parseMany (parseAnyChar ['0'..'9']) rest

parsePair :: Parser a -> Parser (a, a)
parsePair parser input
    | isPar = do
        (result, remaining) <- parser rest
        (_, remChar1) <- parseChar ' ' remaining
        (result2, remaining2) <- parser remChar1
        (_, remaining_str) <- parseChar ')' remaining2
        return ((result, result2), remaining_str)
    | otherwise = Nothing
  where
    isPar = isJust $ parseChar '(' input
    rest = if not isPar then input else tail input

sepBy :: String -> Char -> String
sepBy [] _ = []
sepBy (x:xs) c
    | x == c = xs
    | otherwise = (x:xs)

parseListRec :: Parser a -> Parser [a]
parseListRec parser input = case parser input of
    Nothing -> Just ([], input)
    Just (result, rest) -> do
        case sepBy rest ' ' of
            [] -> do 
                (results, remaining) <- parseListRec parser rest
                Just (result : results, remaining)
            rem -> do
                (results, remaining) <- parseListRec parser rem
                Just (result : results, remaining)

parseList :: Parser a -> Parser [a]
parseList parser input
    | isPar = case parseListRec parser (sepBy rest ' ') of
        Just (result, ')':xs) -> Just (result, xs)
        Just (result, remaining) -> Just (result, remaining)
    | otherwise = Nothing
    where
        isPar = isJust $ parseChar '(' input
        rest = if not isPar then input else tail input

