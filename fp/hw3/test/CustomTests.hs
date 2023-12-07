{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module CustomTests
  (
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
  ) where

import HW3.T1
import HW3.T2
import HW3.T3
import HW3.T4
import Test.HUnit.Base (Test (TestList), (~?=))

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- | Helper: unwrap `Option` type, when its only value.
unwrapOption :: Option a -> a
unwrapOption None     = error "Unwrapped none."
unwrapOption (Some a) = a

-- | Helper: unwrap `Prioritised` type for value.
unwrapPriotisised :: Prioritised a -> a
unwrapPriotisised (Low left)    = left
unwrapPriotisised (Medium left) = left
unwrapPriotisised (High left)   = left

-- | Helper: it denom == 0, then its Error, otherwise - successfully dividing.
safeDivide :: Double -> Double -> Except String Double
safeDivide left right
  | right == 0 = Error "divide by zero"
  | otherwise = Success (left / right)

-- | Helper for testMapPriotisized as Low priority.
low :: String -> String
low left = "Low:" ++ " " ++ left ++ "."

-- | Helper for testMapPriotisized as Medium priority.
med :: String -> String
med left = "Medium:" ++ " " ++ left ++ "."

-- | Helper for testMapPriotisized as High priority.
high :: String -> String
high left = "High:" ++ " " ++ left ++ "."

-- | Helper: run Fun and return result as `a` type.
runFun :: Fun i a -> i -> a
runFun (F f) = f

-- | Test suite: T1, `mapOption`.
testMapOption :: Test
testMapOption =
  TestList [
    unwrapOption (mapOption show (Some 42)) ~?= "42",
    mapOption id (None :: Option Int) ~?= None
  ]

-- | Test suite: T1, `mapPair`.
testMapPair :: Test
testMapPair =
  TestList [
    mapPair (\x -> x - 1) (P 1 2) ~?= P 0 1,
    mapPair read (P "1" "2") ~?= P 1 2
  ]

-- | Test suite: T1, `mapQuad`.
testMapQuad :: Test
testMapQuad =
  TestList [
    mapQuad (/ 2) (Q 4 8 16 32) ~?= Q 2 4 8 16 ,
    mapQuad read (Q "4" "8" "16" "32") ~?= Q 4 8 16 32
  ]

-- | Test suite: T1, `mapAnnotated`.
testMapAnnotated :: Test
testMapAnnotated =
  TestList [
    mapAnnotated show (1 :# 2) ~?= ("1" :# 2),
    mapAnnotated id (1 :# 2) ~?= (1 :# 2)
  ]

-- | Test suite: T1, `mapExcept`.
testMapExcept :: Test
testMapExcept =
  TestList [
    mapExcept show (safeDivide 1 2) ~?= Success (show (1 / 2)),
    mapExcept id (safeDivide 1 0) ~?= Error "divide by zero"
  ]

-- | Test suite: T1, `mapPrioritised`.
testMapPrioritised :: Test
testMapPrioritised =
  TestList [
    unwrapPriotisised (mapPrioritised low (Low "hello")) ~?= "Low: hello.",
    unwrapPriotisised (mapPrioritised med (Medium "world")) ~?= "Medium: world.",
    unwrapPriotisised (mapPrioritised high (High "goodbye")) ~?= "High: goodbye."
  ]

-- | Test suite: T1, `mapList`.
testMapList :: Test
testMapList = TestList [
  mapList show (1 :. (2 :. Nil)) ~?= ("1" :. ("2" :. Nil)),
  mapList (\x -> x * x) (2 :. (4 :. Nil)) ~?= (4 :. (16 :. Nil)),
  mapList (/ 2) Nil ~?= Nil
  ]

-- | Test suite: T1, `mapTree`.
testMapTree :: Test
testMapTree =
  TestList [
    mapTree read (Branch Leaf "1" Leaf) ~?= Branch Leaf 1 Leaf,
    mapTree read (Branch (Branch (Branch Leaf "1" Leaf) "2" (Branch Leaf "3" Leaf)) "4" (Branch (Branch Leaf "5" Leaf) "6" (Branch Leaf "7" Leaf))) ~?= Branch (Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)) 4 (Branch (Branch Leaf 5 Leaf) 6 (Branch Leaf 7 Leaf))
  ]

-- | Test suite: T2, `distOption`.
testDistOption :: Test
testDistOption =
  TestList [
    distOption (None :: Option Int, None :: Option Int) ~?= None,
    distOption (None :: Option Int, Some "thing") ~?= None,
    distOption (Some "thing", None :: Option Int) ~?= None,
    distOption (Some "there is nothing", Some "we can do") ~?= Some ("there is nothing", "we can do")
    ]

-- | Test suite: T2, `wrapOption`.
testWrapOption :: Test
testWrapOption =
  TestList [
    wrapOption 1 ~?= Some 1,
    wrapOption "thing in the way" ~?= Some "thing in the way"
    ]

-- | Test suite: T2, `distPair`.
testDistPair :: Test
testDistPair =
  TestList [
    distPair (P "1" "2", P 1 2) ~?= P ("1", 1) ("2", 2),
    distPair (P 42 24, P 65 56) ~?= P (42, 65) (24, 56) ]

-- | Test suite: T2, `wrapPair`.
testWrapPair :: Test
testWrapPair =
  TestList [
    wrapPair 1 ~?= P 1 1, wrapPair "Saveliy" ~?= P "Saveliy" "Saveliy"
    ]

-- | Test suite: T2, `distQuad`.
testDistQuad :: Test
testDistQuad =
  TestList [
    distQuad (Q "1" "2" "3" "4", Q 4 3 2 1) ~?= Q ("1", 4) ("2", 3) ("3", 2) ("4", 1)
    ]

-- | Test suite: T2, `wrapQuad`.
testWrapQuad :: Test
testWrapQuad =
  TestList [
    wrapQuad "Saveliy" ~?= Q "Saveliy" "Saveliy" "Saveliy" "Saveliy"
    ]

-- | Test suite: T2, `distAnnotated`.
testDistAnnotated :: Test
testDistAnnotated =
  TestList [
    distAnnotated (1 :# "Saveliy", 2 :# "Bakturin") ~?= ((1, 2) :# ("Saveliy" <> "Bakturin")),
    distAnnotated ("There is nothing" :# (mempty :: String), "we can do" :# mempty) ~?= (("There is nothing", "we can do") :# mempty)
    ]

-- | Test suite: T2, `wrapAnnotated`.
testWrapAnnotated :: Test
testWrapAnnotated =
  TestList [
    wrapAnnotated "Ha-ha" ~?= ("Ha-ha" :# (mempty :: String))
    ]

-- | Test suite: T2, `distExcept`.
testDistExcept :: Test
testDistExcept =
  TestList [
    distExcept (safeDivide 1 0, safeDivide 1 0) ~?= Error "divide by zero",
    distExcept (safeDivide 1 0, safeDivide 1 2) ~?= Error "divide by zero",
    distExcept (safeDivide 1 2, safeDivide 1 0) ~?= Error "divide by zero",
    distExcept (safeDivide 1 2, safeDivide 1 3) ~?= Success (1 / 2, 1 / 3)
    ]

-- | Test suite: T2, `wrapExcept`.
testWrapExcept :: Test
testWrapExcept =
  TestList [
    (wrapExcept "fail" :: Except String String) ~?= Success "fail"
    ]

-- | Test suite: T2, `distPrioritised`.
testDistPrioritised :: Test
testDistPrioritised =
  TestList [
    distPrioritised (Low "should be", Low "Low") ~?= Low ("should be", "Low"),
    distPrioritised (Low "should be", Medium "Medium") ~?= Medium ("should be", "Medium"),
    distPrioritised (Low "should be", High "High") ~?= High ("should be", "High"),
    distPrioritised (Medium "should be", Low "Medium") ~?= Medium ("should be", "Medium"),
    distPrioritised (Medium "should be", Medium "Medium") ~?= Medium ("should be", "Medium"),
    distPrioritised (Medium "should be", High "High") ~?= High ("should be", "High"),
    distPrioritised (High "should be", Low "High") ~?= High ("should be", "High"),
    distPrioritised (High "should be", Medium "High") ~?= High ("should be", "High"),
    distPrioritised (High "should be", High "High") ~?= High ("should be", "High")
  ]

-- | Test suite: T2, `wrapPrioritised`.
testWrapPrioritised :: Test
testWrapPrioritised =
  TestList [
    wrapPrioritised "IQ" ~?= Low "IQ"
    ]

-- | Test suite: T2, `distList`.
testDistList :: Test
testDistList =
  TestList [
    distList (1 :. (2 :. Nil), 3 :. (4 :. Nil)) ~?= (1,3) :. ((1,4) :. ((2,3) :. ((2,4) :. Nil))),
    distList ("ha" :. ("Ha" :. Nil), "hA" :. ("HA" :. Nil)) ~?= ("ha","hA") :. (("ha","HA") :. (("Ha","hA") :. (("Ha","HA") :. Nil)))
    ]

-- | Test suite: T2, `wrapList`.
testWrapList :: Test
testWrapList =
  TestList [
    wrapList "ha" ~?= "ha" :. Nil,
    wrapList (1 :. (2 :. Nil)) ~?= (1 :. (2 :. Nil)) :. Nil
    ]

-- | Test suite: T2, `distFun`.
testDistFun :: Test
testDistFun =
  TestList [
    runFun (distFun (F show, F (/ 2))) 42 ~?= ("42.0", 21)
    ]

-- | Test suite: T2, `wrapFun`.
testWrapFun :: Test
testWrapFun =
  TestList [
    runFun (wrapFun "Saveliy Bakturin") (Error "Ignore me") ~?= "Saveliy Bakturin"
    ]

-- | Test suite: T3, `joinOption`.
testJoinOption :: Test
testJoinOption =
  TestList [
    joinOption (Some (Some "Hello")) ~?= Some "Hello"
    ]

-- | Test suite: T3, `joinExcept`.
testJoinExcept :: Test
testJoinExcept =
  TestList [
    joinExcept (Success (Success "No" :: Except String String)) ~?= Success "No"
    ]

-- | Test suite: T3, `joinAnnotated`.
testJoinAnnotated :: Test
testJoinAnnotated =
  TestList [
    joinAnnotated (("Number one" :# "Saveliy ") :# "Bakturin") ~?= ("Number one" :# "BakturinSaveliy "),
    joinAnnotated (([1, 2] :# [3, 4]) :# [5, 6]) ~?= ([1, 2] :# [5, 6, 3, 4])
    ]

-- | Test suite: T3, `joinList`.
testJoinList :: Test
testJoinList =
  TestList [
    joinList ((1 :. 2 :. Nil) :. (3 :. 4 :. Nil) :. Nil) ~?= 1 :. (2 :. (3 :. (4 :. Nil))),
    joinList (Nil :: List (List Integer)) ~?= Nil
    ]

-- | Test suite: T3, `joinList`.
testJoinFun :: Test
testJoinFun =
  TestList [
    runFun (joinFun (wrapFun (wrapFun "Saveliy Bakturin"))) (Error "Ignore me") ~?= "Saveliy Bakturin"
    ]

-- | Test suite: T4, `eval`.
testEval :: Test
testEval =
  TestList [
    runS (eval (1 + 2)) [] ~?= (3 :# [Add 1 2]),
    runS (eval (1 + 2 * 3 - 5)) [] ~?= (2.0 :# [Sub 7 5, Add 1 6, Mul 2 3]),
    runS (eval (1 + 2 * 3 - 5)) [Add 999 999] ~?= (2.0 :# [Sub 7 5, Add 1 6, Mul 2 3, Add 999 999]),
    runS (mapState (/ 2) (eval (1 + 2 * 3 - 5))) [] ~?= (1.0 :# [Sub 7 5, Add 1 6, Mul 2 3])
  ]
