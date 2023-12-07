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
    testMapOption
    , testMapPair
    , testMapQuad
    , testMapAnnotated
    , testMapExcept
    , testMapPrioritised
    , testMapList
    , testMapTree
    , testDistOption
    , testWrapOption
    , testDistPair
    , testWrapPair
    , testDistQuad
    , testWrapQuad
    , testDistAnnotated
    , testWrapAnnotated
    , testDistExcept
    , testWrapExcept
    , testDistPrioritised
    , testWrapPrioritised
    , testDistList
    , testWrapList
    , testDistFun
    , testWrapFun
    , testJoinOption
    , testJoinExcept
    , testJoinAnnotated
    , testJoinList
    , testJoinFun
    , testEval
    ]

main :: IO Counts
main = runTestTT tests
