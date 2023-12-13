import Test.HUnit
import qualified System.Exit

test1 :: Test
test1 = TestCase (assertEqual "should return 3" (3 :: Integer) ((+) 1 2))

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then System.Exit.exitFailure else System.Exit.exitSuccess
