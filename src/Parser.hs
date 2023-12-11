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