{-# OPTIONS_GHC -Wno-type-defaults #-}

module CustomTests
  ( testTFoldr,
    testSplitOn,
    testJoinWith,
    testMcat,
    testEpart,
    testListPlus,
    testDotString,
    testFun,
  )
where

import Data.List.NonEmpty
import Data.Monoid
import HW2.T1
import HW2.T2
import HW2.T3
import HW2.T4
import Test.HUnit.Base (Test (TestList), (~?=))

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- Helper: using `tfoldr` to create new List from Tree.
treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []

-- Helper: using `tfoldr` to find maximum in Tree.
treeFindMaxBadly :: (Num a, Ord a) => Tree a -> a
treeFindMaxBadly = tfoldr max 0

-- Helper: using `tfoldr` to find minimum in Tree.
treeFindMinBadly :: (Num a, Ord a) => Tree a -> a
treeFindMinBadly = tfoldr min 0

-- Test: feature `tfoldr` from HW2::T1.
testTFoldr :: Test
testTFoldr =
  TestList
    [ treeToList (Branch 1 Leaf (1 :: Integer) Leaf) ~?= [1],
      treeToList (Branch 3 (Branch 1 Leaf ((-1) :: Integer) Leaf) 0 (Branch 1 Leaf 1 Leaf)) ~?= [-1, 0, 1],
      treeFindMaxBadly (Branch 14 (Branch 6 (Branch 2 Leaf ((-4) :: Integer) (Branch 1 Leaf (-3) Leaf)) (-1) (Branch 3 (Branch 1 Leaf 1 Leaf) 2 (Branch 1 Leaf 4 Leaf))) 6 (Branch 7 (Branch 3 (Branch 1 Leaf 7 Leaf) 43 (Branch 1 Leaf 123 Leaf)) 234 (Branch 3 (Branch 1 Leaf 645 Leaf) 1234 (Branch 1 Leaf 7653 Leaf)))) ~?= 7653,
      treeFindMaxBadly (Branch 10 (Branch 5 (Branch 3 (Branch 1 Leaf ((-9) :: Integer) Leaf) (-8) (Branch 1 Leaf (-7) Leaf)) (-6) (Branch 1 Leaf (-5) Leaf)) (-4) (Branch 4 (Branch 1 Leaf (-3) Leaf) (-2) (Branch 2 Leaf (-1) (Branch 1 Leaf 0 Leaf)))) ~?= 0,
      treeFindMinBadly (Branch 14 (Branch 6 (Branch 2 Leaf ((-4) :: Integer) (Branch 1 Leaf (-3) Leaf)) (-1) (Branch 3 (Branch 1 Leaf 1 Leaf) 2 (Branch 1 Leaf 4 Leaf))) 6 (Branch 7 (Branch 3 (Branch 1 Leaf 7 Leaf) 43 (Branch 1 Leaf 123 Leaf)) 234 (Branch 3 (Branch 1 Leaf 645 Leaf) 1234 (Branch 1 Leaf 7653 Leaf)))) ~?= -4,
      treeFindMinBadly (Branch 10 (Branch 5 (Branch 3 (Branch 1 Leaf ((-9) :: Integer) Leaf) (-8) (Branch 1 Leaf (-7) Leaf)) (-6) (Branch 1 Leaf (-5) Leaf)) (-4) (Branch 4 (Branch 1 Leaf (-3) Leaf) (-2) (Branch 2 Leaf (-1) (Branch 1 Leaf 0 Leaf)))) ~?= -9
    ]

-- Test: feature `splitOn` from HW2::T2.
testSplitOn :: Test
testSplitOn =
  TestList
    [ splitOn '/' "" ~?= ("" :| []),
      splitOn '/' "/" ~?= ("" :| [""]),
      splitOn '/' "//" ~?= ("" :| ["", ""]),
      splitOn '/' "saveliy" ~?= ("saveliy" :| []),
      splitOn '/' "saveliy/bakturin" ~?= ("saveliy" :| ["bakturin"]),
      splitOn '/' "/saveliy/bakturin" ~?= ("" :| ["saveliy", "bakturin"]),
      splitOn '/' "/saveliy/bakturin/" ~?= ("" :| ["saveliy", "bakturin", ""])
    ]

-- Test: feature `joinWith` from HW2::T2.
testJoinWith :: Test
testJoinWith =
  TestList
    [ joinWith '/' (splitOn '/' "") ~?= "",
      joinWith '/' (splitOn '/' "/") ~?= "/",
      joinWith '/' (splitOn '/' "//") ~?= "//",
      joinWith '/' (splitOn '/' "saveliy") ~?= "saveliy",
      joinWith '/' (splitOn '/' "saveliy/bakturin") ~?= "saveliy/bakturin",
      joinWith '/' (splitOn '/' "/saveliy/bakturin") ~?= "/saveliy/bakturin",
      joinWith '/' (splitOn '/' "/saveliy/bakturin/") ~?= "/saveliy/bakturin/"
    ]

-- Test: feature `mcat` from HW2::T3.
testMcat :: Test
testMcat =
  TestList
    [ mcat [Just "mo", Nothing, Nothing, Just "no", Just "id"] ~?= "monoid",
      Data.Monoid.getSum (mcat [Nothing, Just 2, Nothing, Just 40]) ~?= 42,
      Data.Monoid.getProduct (mcat [Just (Product 3), Just (Product 4), Just (Product 2)]) ~?= 24,
      mcat [Just [1, 2, 3], Just [4, 5], Just [6, 7, 8]] ~?= [1, 2, 3, 4, 5, 6, 7, 8]
    ]

-- Test: feature `epart` from HW2::T3.
testEpart :: Test
testEpart =
  TestList
    [ epart [Left (Sum 3), Right [1, 2, 3], Left (Sum 5), Right [4, 5]] ~?= (Sum {getSum = 8}, [1, 2, 3, 4, 5]),
      epart [Left "Hello", Right (Product 5), Right (Product 2), Left "World"] ~?= ("HelloWorld", Product {getProduct = 10})
    ]

-- Helper: generate ListPlus from only non-empty List, otherwise - error.
listToListPlus :: [a] -> ListPlus a
listToListPlus [x]      = HW2.T4.Last x
listToListPlus (x : xs) = x :+ listToListPlus xs
listToListPlus _        = error "Bad using listToListPlus function"

-- Test: feature Semigroup of `ListPlus` from HW2::T4.
testListPlus :: Test
testListPlus =
  TestList
  [
    listToListPlus [1, 2, 3] <> listToListPlus [4, 5, 6] ~?= listToListPlus [1, 2, 3, 4, 5, 6],
    (listToListPlus [1, 2, 3] <> listToListPlus [4, 5, 6]) <> listToListPlus [7, 8, 9] ~?= listToListPlus [1, 2, 3] <> (listToListPlus [4, 5, 6] <> listToListPlus [7, 8, 9])
  ]

-- Test: feature Semigroup and Monoid of `DotString` from HW2::T4.
testDotString :: Test
testDotString =
  TestList
  [
    DS "person" <> DS "address" <> DS "city" ~?= DS "person.address.city",
    (DS "java" <> DS "util") <> (DS "concurrent" <> DS "ThreadPoolExecutor") ~?= DS "java" <> (DS "util" <> DS "concurrent") <> DS "ThreadPoolExecutor",
    DS "matplotlib" <> mempty ~?= mempty <> DS "matplotlib"
  ]

-- Helper: apply Fun from HW2::T4.
funApply :: Fun a -> a -> a
funApply (F f) = f

-- Helper: Fun to sum with N.
funAddToN :: Int -> Fun Int
funAddToN n = F (+ n)

-- Helper: Fun to multiply with N.
funMulToN :: Int -> Fun Int
funMulToN n = F (* n)

-- Test: feature Semigroup and Monoid of `Fun` from HW2::T4.
testFun :: Test
testFun =
  TestList
    [ funApply (funAddToN 999 <> funMulToN 999) 1 ~?= 999 + 999,
      funApply (funMulToN 999 <> funAddToN 999) 0 ~?= 999 * 999,
      funApply (funAddToN 5 <> (funMulToN 6 <> funAddToN 9)) 42 ~?= funApply ((funAddToN 5 <> funMulToN 6) <> funAddToN 9) 42,
      funApply (funAddToN 5 <> mempty) 42 ~?= 47,
      funApply (mempty <> funAddToN 5) 42 ~?= 47
    ]
