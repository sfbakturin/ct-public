{-# OPTIONS_GHC -Wno-type-defaults #-}

module CustomTests
  (
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
  ) where

import HW0.T1
import HW0.T3
import HW0.T4
import HW0.T5
import HW0.T6
import Test.HUnit.Base (Test (TestList), (~?=), (~:))

funIntToStr :: Int -> String
funIntToStr _ = "hello"

funStrToInt :: String -> Int
funStrToInt _ = 420

testDistrib :: Test
testDistrib =
  TestList [
    "Distribution of non-pair type" ~: distrib (Left "123" :: Either String (Int, Int)) ~?= (Left "123", Left "123"),
    "Distribution of pair type" ~: distrib (Right (123, "321") :: Either String (Int, String)) ~?= (Right 123, Right "321")
  ]

testFlipIso :: Test
testFlipIso =
  TestList [
    "Flip once" ~: runIso (flipIso (Iso funIntToStr funStrToInt)) "dummy" ~?= 420,
    "Flip twice" ~: runIso (flipIso (flipIso (Iso funIntToStr funStrToInt))) 420 ~?= "hello"
  ]

testCompose :: Test
testCompose =
  TestList [
    "Compose 1" ~: compose read show 123 ~?= (123 :: Integer),
    "Compose 2" ~: compose read (++ "321") "123" ~?= (123321 :: Integer)
  ]

testContract :: Test
testContract =
  TestList [
    "Contract 1" ~: contract (\l r -> l ^ 2 + r ^ 2) 123 ~?= 30258,
    "Contract 2" ~: contract (\l r -> read (l ++ r)) "123" ~?= 123123
  ]

testI :: Test
testI =
  TestList [
    "I 1" ~: i "123" ~?= "123",
    "I 2" ~: i (123 :: Integer) ~?= 123
  ]

testK :: Test
testK =
  TestList [
    "K 1" ~: k (1 :: Integer) "2" ~?= 1,
    "K 2" ~: k "1" (2 :: Integer) ~?= "1"
  ]

testPermute :: Test
testPermute =
  TestList [
    "Permute 1" ~: permute (\l r -> l : [r]) 123 321 ~?= [321, 123],
    "Permute 2" ~: permute (,) 123 321 ~?= (321, 123)
  ]

testS :: Test
testS =
  TestList [
    "S 1" ~: s (+) (* 2) (2 :: Integer) ~?= 6,
    "S 2" ~: s (++) (const "456") "123" ~?= "123456"
  ]


testFac :: Test
testFac =
  TestList [
    "Fac (small number)" ~: fac 12 ~?= 479001600,
    "Fac (big number)" ~: fac 123 ~?= 12146304367025329675766243241881295855454217088483382315328918161829235892362167668831156960612640202170735835221294047782591091570411651472186029519906261646730733907419814952960000000000000000000000000000
  ]

testFib :: Test
testFib =
  TestList [
    "Fib (small number)" ~: fib 5 ~?= 5,
    "Fib (big number)" ~: fib 43 ~?= 433494437
  ]

testMap :: Test
testMap =
  TestList [
    "Map 1" ~: map' (* 2) [1, 2, 3] ~?= map (* 2) [1, 2, 3],
    "Map 2" ~: map' (\x -> show x ++ "123") [1, 2, 3] ~?= map (\x -> show x ++ "123") [1, 2, 3]
  ]

testRepeat :: Test
testRepeat =
  TestList [
    "Repeat 1" ~: take 4 (repeat' 1) ~?= replicate 4 1,
    "Repeat 2" ~: take 5 (repeat' "1") ~?= replicate 5 "1"
  ]

taskCalculations :: Test
taskCalculations =
  TestList [
    "Sum of" ~: nToNum (nplus (nFromNatural 1) (nFromNatural 2)) ~?= 3,
    "Multiply of" ~: nToNum (nmult (nFromNatural 2) (nFromNatural 14)) ~?= 28
  ]

testWHNF :: Test
testWHNF =
  TestList [
    "b" ~: b ~?= b_whnf,
    "c" ~: c ~?= c_whnf
  ]
