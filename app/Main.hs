module Main (main) where
import System.Environment (getArgs)
import Lib (readExpr, eval)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head