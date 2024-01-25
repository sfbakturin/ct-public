import CustomTests
import Test.HUnit (runTestTT)
import Test.HUnit.Base (Counts, Test (TestList))

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

tests :: Test
tests =
  TestList [
    testCHT
  ]

main :: IO Counts
main = runTestTT tests
