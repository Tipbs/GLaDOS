module Main (main) where
import System.Environment (getArgs)
import Lib (readExpr, eval, trapError, extractValue)

main :: IO ()
main = do
    args <- getArgs
    let evaled = fmap show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled
