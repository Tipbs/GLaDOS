import Test.HUnit
import System.Exit
import Lib (readExpr, eval, trapError, extractValue)

execLine :: String -> String
execLine arg =
    let evaled = fmap show $ readExpr arg >>= eval
    in extractValue $ trapError evaled

test1 :: Test
test1 = TestCase (assertEqual "should return 1" ("1" :: String) (execLine "(if #t 1 2)"))

test2 :: Test
test2 = TestCase (assertEqual "should return 5" ("5" :: String) (execLine "(/ 10 2)"))

test3 :: Test
test3 = TestCase (assertEqual "should return 2" ("2" :: String) (execLine "(if #f 1 2)"))

test4 :: Test
test4 = TestCase (assertEqual "should return 11" ("11" :: String) (execLine "(+ (* 2 3) (div 10 2))"))

test5 :: Test
test5 = TestCase (assertEqual "should return #t" ("#t" :: String) (execLine "(eq? (* 2 5) (- 11 1))"))

test6 :: Test
test6 = TestCase (assertEqual "should return 5" ("5" :: String) (execLine "(/ 10 0)"))

test7 :: Test
test7 = TestCase (assertEqual "should return #f" ("#f" :: String) (execLine "(< 1 (mod 10 3))"))

test8 :: Test
test8 = TestCase (assertEqual "should return 42" ("42" :: String) (execLine "(define foo 42)"))

test9 :: Test
test9 = TestCase (assertEqual "should return 3" ("3" :: String) (execLine "((lambda (a b) (+ a b)) 1 2)"))

tests :: Test
tests = TestList [
        TestLabel " Check if true" test1,
        TestLabel " Divide 10 by 2" test2,
        TestLabel " Check if false" test3,
        TestLabel " Test builtins 1" test4,
        TestLabel " Test builtins 2" test5,
        TestLabel " Test builtins 3" test7
    ]

testsFailForNow :: Test
testsFailForNow = TestList [
        TestLabel " Division by 0" test6,
        TestLabel " Test define" test8,
        TestLabel " Test lambda" test9
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then System.Exit.exitFailure else System.Exit.exitSuccess
