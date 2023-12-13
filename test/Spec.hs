import Test.HUnit
import qualified System.Exit
import Lib (readExpr, eval, trapError, extractValue)

execLine :: String -> String
execLine arg =
    let evaled = fmap show $ readExpr arg >>= eval
    in extractValue $ trapError evaled

test1 :: Test
test1 = TestCase (assertEqual "should return 1" ("1" :: String) (execLine "(if #t 1 2)"))

tests :: Test
tests = TestList [
        TestLabel "Check if and boolean" test1
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then System.Exit.exitFailure else System.Exit.exitSuccess
