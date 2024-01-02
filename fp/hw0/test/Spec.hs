import CustomTests
import Test.HUnit (runTestTT)
import Test.HUnit.Base (Counts, Test (TestList))

tests :: Test
tests =
  TestList [
    testDistrib,
    testFlipIso,
    testCompose,
    testContract,
    testI,
    testK,
    testPermute,
    testS,
    testFac,
    testFib,
    testMap,
    testRepeat,
    taskCalculations,
    testWHNF
  ]

main :: IO Counts
main = runTestTT tests
