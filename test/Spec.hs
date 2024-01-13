import Test.HUnit
import System.Exit
import Wasm (magic, version, buildSectionHeader, buildWasm, compileOp, compileExpr, buildDataSec, buildDataSegments, buildSegmentHeader, getIdData, compileGetLocalVar)
import WasmNumber (buildNumber, decodeNumber, buildWords, buildString)
import KopeParserLib (KopeVal (..))

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
testFunctionCall = TestCase (assertEqual "wrong output for good function call" (Right ([0x41, 0x5, 0x10, 0x00], [], [])) (compileExpr (KopeArray [KopeAtom "add", KopeNumber 5]) [KopeFunc "add" ["a", "b"] [KopeNumber 5], KopeFunc "sub" ["a", "b"] [KopeNumber 5]] [] []))

testFunctionCallSnd :: Test
testFunctionCallSnd = TestCase (assertEqual "wrong output for good function call 2" (Right ([0x41, 0xA, 0x10, 0x01], [], [])) (compileExpr (KopeArray [KopeAtom "sub", KopeNumber 10]) [KopeFunc "add" ["a", "b"] [KopeNumber 5], KopeFunc "sub" ["a", "b"] [KopeNumber 5]] [] []))

testFunctionCallWrong :: Test
testFunctionCallWrong = TestCase (assertEqual "wrong output for bad function call" (Left "Could not find function named null") (compileExpr (KopeArray [KopeAtom "null", KopeNumber 10]) [KopeFunc "add" ["a", "b"] [KopeNumber 5], KopeFunc "sub" ["a", "b"] [KopeNumber 5]] [] []))

testBuildWords :: Test
testBuildWords = TestCase (assertEqual "buildWords for 624485" [101, 14, 38] (buildWords 624485))

testCompileGetLocalVar :: Test -- Either String ([Word8], [(String, Int)], [Data])
testCompileGetLocalVar = TestCase (assertEqual "test get local var with 3 locals" (Right ([0x20, 0x3], [("a", 5), ("b", 10), ("c", 15)], [])) (compileGetLocalVar "c" [("a", 5), ("b", 10), ("c", 15)] []))

-- 1001 1000 0111 0110 0101
-- 1001 1000 0111 0110 0101
testBuildUnsignedNumber :: Test
testBuildUnsignedNumber = TestCase (assertEqual "wrong buildNumber output with 0x65" [0x65] (buildNumber 0x65))

testBuildUnsignedNumber2 :: Test
testBuildUnsignedNumber2 = TestCase (assertEqual "wrong buildNumber output with 624485" [0x26, 0x8E, 0xE5] (buildNumber 624485))

testBuildUnsignedNumber3 :: Test
testBuildUnsignedNumber3 = TestCase (assertEqual "wrong buildNumber output with 3" [0x3] (buildNumber 0x3))

-- compileExpr (List [Atom "add", Number 5]) [Func "add" ["a", "b"] [Number 5], Func "sub" ["a", "b"] [Number 5]] []
testSimpleBuildWasm :: Test
testSimpleBuildWasm = TestCase (assertEqual "Wrong buildwasm for simple with sub func" (Right bytes) (buildWasm [KopeFunc "sub" ["a", "b"] b]))
    where
        bytes = [
            0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, 0x01, 0x07, 0x01, 0x60, 0x02, 0x7F, 0x7F, 0x01, 
            0x7F, 0x03, 0x02, 0x01, 0x00, 0x0A, 0x09, 0x01, 0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x6B, 0x0B]
        b = [KopeArray [KopeAtom "-", KopeAtom "a", KopeAtom "b"]]

testSimpleBuildWasm2 :: Test
testSimpleBuildWasm2 = TestCase (assertEqual "Wrong buildwasm for simple with sub func" (Right bytes) (buildWasm [KopeFunc "sub" ["a", "b"] subB, KopeFunc "callSub" [] callsubB]))
    where
        bytes = [
                0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0B, 0x02, 0x60, 0x02, 0x7F, 0x7F, 0x01, 
                0x7F, 0x60, 0x00, 0x01, 0x7F, 0x03, 0x03, 0x02, 0x00, 0x01, 0x0A, 0x12, 0x02, 0x07, 0x00, 0x20, 
                0x00, 0x20, 0x01, 0x6B, 0x0B, 0x08, 0x00, 0x41, 0x0A, 0x41, 0x06, 0x10, 0x00, 0x0B
            ]
        subB = [KopeArray [KopeAtom "-", KopeAtom "a", KopeAtom "b"]]
        callsubB = [KopeArray [KopeAtom "sub", KopeNumber 10, KopeNumber 6]]

-- Right [0,97,115,109,1,0,0,0,1,7,1,96,2,127,127,1,127,3,2,1,0,5,0,32,0,32,1,107,11]
-- ["0","61","73","6d","1","0","0","0","1","7","1","60","2","7f","7f","1","7f","3","2","1","0","a","8","1","5","0","20","0","20","1","6b","b"]

testBuildString :: Test
testBuildString = TestCase (assertEqual "wrong buildString output with HelloWorld!" [0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x00] (buildString "Hello World!"))

testGetIdData :: Test
testGetIdData = TestCase (assertEqual "wrong getIdData output with HelloWorld!" 1 (getIdData 0 (KopeString "Hello World!") [KopeString "Test", KopeString "Hello World!"]))

testBuildSegmentHeader :: Test
testBuildSegmentHeader = TestCase (assertEqual "wrong buildSegmentHeader output with HelloWorld!" [0x00, 0x41, 0x00, 0x0b] (buildSegmentHeader 0))

testBuildDataSegments :: Test
testBuildDataSegments = TestCase (assertEqual "wrong buildDataSegments output with HelloWorld!" [0x00, 0x41, 0x00, 0x0b, 0x0d, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x00] (buildDataSegments (KopeString "Hello World!") [KopeString "Hello World!"]))

testBuildDataSec :: Test
testBuildDataSec = TestCase (assertEqual "wrong buildDataSec output with HelloWorld!" [0x0b, 0x13, 0x01, 0x00, 0x41, 0x00, 0x0b, 0x0d, 0x48, 0x65 , 0x6c, 0x6c , 0x6f, 0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x00] (buildDataSec [KopeString "Hello World!"]))


wasmTests :: Test
wasmTests = TestList [
        TestLabel "check magic value" testMagic,
        TestLabel "check version value" testVersion,
        TestLabel "build section header as an function section" testBuildSectionHeader,
        TestLabel "build op with '+' with CompileOp" testCompileOp,
        TestLabel "build unsigned words for 0x65 to ULEB128" testBuildWords,
        TestLabel "build unsigned number 0x65 to ULEB128" testBuildUnsignedNumber,
        TestLabel "build unsigned number 624485 to ULEB128" testBuildUnsignedNumber2,
        TestLabel "build unsigned number 3 to ULEB128" testBuildUnsignedNumber3,
        TestLabel "build successful function call" testFunctionCall,
        TestLabel "build successful function call 2" testFunctionCallSnd,
        TestLabel "build unsuccessful function call" testFunctionCallWrong,
        TestLabel "build simple program with one function" testSimpleBuildWasm,
        TestLabel "build simple program with 2 functions calling each other" testSimpleBuildWasm2,
        TestLabel "build string with hello world" testBuildString,
        TestLabel "get string id with hello world" testGetIdData,
        TestLabel "build segment header with hello world" testBuildSegmentHeader,
        TestLabel "build data segments with hello world" testBuildDataSegments,
        TestLabel "build get local var with 3 locals" testCompileGetLocalVar,
        TestLabel "build data with hello world" testBuildDataSec
    ]

main :: IO ()
main = do
    results <- runTestTT wasmTests
    if failures results > 0 then System.Exit.exitFailure else System.Exit.exitSuccess
