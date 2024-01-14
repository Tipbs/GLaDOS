module KopeTests (kopeTests) where
import Test.HUnit
import System.Exit
import KopeParserLib
import KopeParser

testCharP :: Test
testCharP = TestList [
  runParser (charP 'a') "abc" ~=? Just ("bc", 'a'),
  runParser (charP 'a') "bc" ~=? Nothing
  ]

testStringP :: Test
testStringP = TestList [
  runParser (stringP "ab") "abc" ~=? Just ("c", "ab"),
  runParser (stringP "ab") "acb" ~=? Nothing
  ]

testWs :: Test
testWs = runParser ws "    \nd" ~=? Just ("d", "    \n")

testNotNull :: Test
testNotNull = TestList [
    runParser (notNull ws) "" ~=? Nothing,
    runParser (notNull ws) " " ~=? Just ("", " ")
  ]

testOneOfP :: Test
testOneOfP = TestList [
  runParser (oneOfP ["=", "*"]) "*" ~=? Just ("", KopeAtom "*"),
  runParser (oneOfP ["=", "*"]) "-" ~=? Nothing
  ]

testLetterP :: Test
testLetterP = TestList [
  runParser letterP "set_value" ~=? Just ("", "set_value"),
  runParser letterP "?" ~=? Nothing
  ]

testKopeExpr :: Test
testKopeExpr = TestList [
  runParser kopeExpr "fn set_value(arg, arg) { if (1 == 2) { return 1; }; print(\"Hello\"); }" ~=?
  Just ("",KopeFunc {funcName = "set_value", funcParams = ["arg","arg"], funcBody = [KopeArray [KopeAtom "if",KopeArray [KopeAtom "==",KopeNumber 1,KopeNumber 2],KopeArray [KopeArray [KopeAtom "return",KopeNumber 1]]],KopeArray [KopeAtom "print",KopeString "Hello"]]})
  ]

testShowKopeVal :: Test
testShowKopeVal = TestList [
  show (KopeString "test") ~=?
  "KopeString \"test\""
  ]

testEqKopeVal :: Test
testEqKopeVal = TestList [
    KopeBool True == KopeBool True ~=? True,
    KopeBool True == KopeBool False ~=? False
  ]

kopeTests :: Test
kopeTests = TestList [
  TestLabel "Test charP" testCharP,
  TestLabel "Test stringP" testStringP,
  TestLabel "Test ws" testWs,
  TestLabel "Test notNull" testNotNull,
  TestLabel "Test oneOfP" testOneOfP,
  TestLabel "Test letterP" testLetterP,
  TestLabel "Test kopeExpr" testKopeExpr,
  TestLabel "Test show KopeVal" testShowKopeVal,
  TestLabel "Test eq KopeVal" testEqKopeVal
  ]
  
