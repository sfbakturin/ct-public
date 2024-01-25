{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module CustomTests
  (
    testCHT
  ) where

import Control.Concurrent.Classy (fork)
import Control.Concurrent.Classy.Async (async, wait, withAsync)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe, isNothing)
import HW6.T1
import HW6.T2 (Add, Contains, Delete, TSet)
import Test.HUnit.Base (Test (TestCase, TestList), assertBool, assertEqual, assertFailure, (~:),
                        (~?=))

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data Proxy a = Proxy

type ExampleSet1 = '[ "a", "b" ] :: TSet
type ExampleSet2 = '[ "b" ] :: TSet

test1 :: Proxy 'True
test1 = Proxy @(Contains "a" ExampleSet1)

test2 :: Proxy '[ "b" ]
test2 = Proxy @(Delete "a" ExampleSet1)

test3 :: Proxy '[ "b", "a" ]
test3 = Proxy @(Add "a" ExampleSet2)

testCHT :: Test
testCHT =
  TestList [
    "Init `ConcurrentHashMap`." ~: TestCase (do
      cht <- newCHT
      size <- sizeCHT cht
      assertEqual "Initialized CHT should be zero-sized." 0 size
    ),
    "Empty `ConcurrentHashMap`." ~: TestCase (do
      cht <- newCHT
      a <- getCHT "key1" cht
      b <- getCHT "key2" cht
      assertBool "Empty CHT should return `Nothing` to any `getCHT`." (isNothing a)
      assertBool "Empty CHT should return `Nothing` to any `getCHT`." (isNothing b)
    ),
    "Manipulations with `ConcurrentHashMap`." ~: TestCase (do
      let rng = [0..10 :: Int]
      let toStr x = "value" ++ show x
      cht <- newCHT
      forM_ rng (\(i :: Int) -> do
                          putCHT i (toStr i) cht
        )
      rawActual <- mapM (`getCHT` cht) rng
      let expected = map (\i -> (i, toStr i)) rng
      let actual = map (\(i, v) -> case v of
                                    Nothing -> (i, "No element was found.")
                                    Just ve -> (i, ve)) (zip rng rawActual :: [(Int, Maybe String)])
      size <- sizeCHT cht
      assertEqual "Basic put and get to CHT should be sized to 11" 11 size
      assertEqual "Basic put and get to CHT should work." expected actual
    ),
    "Manipulations with a huge capacity in `ConcurrentHashMap`." ~: TestCase (do
      let rng = [0..100 :: Int]
      let toStr x y = "value" ++ "[" ++ show x ++ "," ++ show y ++ "]"
      let sizeExpected = length rng
      cht <- newCHT
      forM_ rng (\(z :: Int) -> do
                          forM_ rng (\(y :: Int) -> do
                                              forM_ rng (\(x :: Int) -> do
                                                                  putCHT z (toStr x y) cht
                                                )
                            )
        )
      rawActual <- mapM (`getCHT` cht) rng
      let expected = map (, "value[100,100]") rng
      let actual = map (\(i, v) -> case v of
                                    Nothing -> (i, "No element was found.")
                                    Just ve -> (i, ve)) (zip rng rawActual :: [(Int, Maybe String)])
      sizeActual <- sizeCHT cht
      assertEqual "Huge put and get to CHT should be sized right." sizeExpected sizeActual
      assertEqual "Huge put and get to CHT should work." expected actual
    ),
    "Deja Fu." ~: TestCase (do
      let rng = [0..100 :: Int]
      let toStr x = "valued in parallelism=" ++ show x
      let sizeExpected = length rng
      cht <- newCHT
      withAsync (forM_ rng (\(x :: Int) -> do
                          fork (forM_ rng (\(y :: Int) -> do
                                              let k = (x * sizeExpected) + y
                                              let expectedValue = toStr k
                                              withAsync (putCHT k expectedValue cht) (\asn -> do
                                                                                            wait asn
                                                                                            placed <- getCHT k cht
                                                                                            let actualValue = fromMaybe "There is nothing we can do." placed
                                                                                            assertEqual "In parallelism there should work get." expectedValue actualValue
                                                )
                            ))
        )) (\x -> do
                  wait x
        )
    )
  ]
