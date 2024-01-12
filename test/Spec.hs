import Test.HUnit
import System.Exit
import Wasm (magic, version, buildSectionHeader, buildWasm, compileOp, compileExpr, buildDataSec)
import WasmNumber (buildNumber, decodeNumber, buildWords)
import Parser (LispVal (..))

testMagic :: Test
testMagic = TestCase (assertEqual "wrong magic value" ([0x00, 0x61, 0x73, 0x6d]) magic)

testVersion :: Test
testVersion = TestCase (assertEqual "wrong version value" ([0x01, 0x00, 0x00, 0x00]) version)

testBuildSectionHeader :: Test
testBuildSectionHeader = TestCase (assertEqual "wrong buildSectionHeader output" [0x03, 0x08, 0x01] (buildSectionHeader 0x03 0x07 1))

testCompileOp :: Test
testCompileOp = TestCase (assertEqual "wrong compileOp output" [0x6a] (compileOp "+"))

-- compileExpr (List [Atom "add", Number 5]) [Func "add" ["a", "b"] [Number 5], Func "sub" ["a", "b"] [Number 5]] []
testFunctionCall :: Test
testFunctionCall = TestCase (assertEqual "wrong output for good function call" (Right ([0x41, 0x5, 0x10, 0x00], [], [])) (compileExpr (List [Atom "add", Number 5]) [Func "add" ["a", "b"] [Number 5], Func "sub" ["a", "b"] [Number 5]] [] []))

testBuildWords :: Test
testBuildWords = TestCase (assertEqual "buildWords for 624485" [101, 14, 38] (buildWords 624485))

-- 1001 1000 0111 0110 0101
-- 1001 1000 0111 0110 0101
testBuildUnsignedNumber :: Test
testBuildUnsignedNumber = TestCase (assertEqual "wrong buildNumber output with 0x65" [0x65] (buildNumber 0x65))

testBuildUnsignedNumber2 :: Test
testBuildUnsignedNumber2 = TestCase (assertEqual "wrong buildNumber output with 624485" [0x26, 0x8E, 0xE5] (buildNumber 624485))

testBuildDataSec :: Test
testBuildDataSec = TestCase (assertEqual "wrong buildNumber output with 624485" [0x0b, 0x13, 0x01, 0x00, 0x41, 0x00, 0x0b, 0x0d, 0x48, 0x65 , 0x6c, 0x6c , 0x6f, 0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x00] (buildDataSec [String "Hello World!"]))

wasmTests :: Test
wasmTests = TestList [
        TestLabel "check magic value" testMagic,
        TestLabel "check version value" testVersion,
        TestLabel "build section header as an function section" testBuildSectionHeader,
        TestLabel "build op with '+' with CompileOp" testCompileOp,
        TestLabel "build unsigned words for 0x65 to ULEB128" testBuildWords,
        TestLabel "build unsigned number 0x65 to ULEB128" testBuildUnsignedNumber,
        TestLabel "build unsigned number 624485 to ULEB128" testBuildUnsignedNumber2,
        TestLabel "build successful function call" testFunctionCall, 
        TestLabel "build data with hello world" testBuildDataSec
    ]

main :: IO ()
main = do
    results <- runTestTT wasmTests
    if failures results > 0 then System.Exit.exitFailure else System.Exit.exitSuccess
