module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (MonadConc (atomically), MonadSTM (newTVar, writeTVar), STM)
import Control.Concurrent.Classy.STM (TArray, TVar)
import Control.Concurrent.Classy.STM.TVar (readTVar)
import Control.Monad (when)
import Data.Array.MArray (getBounds, getElems, newArray, readArray, writeArray)
import Data.Hashable (Hashable (hash))

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- | Initializing capacity for `ConcurrentHashMap`.
-- Returns number of preallocated elements.
initCapacity :: Int
initCapacity = 16

-- | Boundary limit for reallocating elements capacity.
-- Returns coefficient of boundary.
loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

-- | Hash-function for `ConcurrentHashMap`.
-- Return collision index by formula "hash(k) % capacity".
hashCHT :: Hashable k => k     -- ^ `k` argument of hashable key in map.
                        -> Int -- ^ `Int` argument of map's capacity.
                        -> Int -- ^ `Int` return value.
hashCHT key = mod (hash key)

-- | Constructs new `ConcurrentHashMap` with `initCapacity` preallocated elements.
newCHT :: MonadConc m => m (CHT (STM m) k v) -- ^ `CHT (STM m) k v` return value.
newCHT = atomically (do
    initializedStorage <- newArray (0, initCapacity - 1) []
    buckets <- newTVar initializedStorage
    size <- newTVar 0
    return (CHT buckets size)
  )

-- | Returns the value to which the specified key is mapped.
-- If map contains for the key, the returns `Just` valued.
-- Otherwise, returns `Nothing`.
getCHT :: (MonadConc m, Eq k, Hashable k) => k                   -- ^ `k` argument key.
                                              -> CHT (STM m) k v -- ^ `CHT (STM m) k v` argument map.
                                              -> m (Maybe v)     -- ^ `Maybe v` return value.
getCHT key cht = atomically (do
    cap <- capacityCHT cht
    buckets <- readTVar (chtBuckets cht)
    collided <- readArray buckets (hashCHT key cap)
    return (lookup key collided)
  )

-- | Associates the specified value with the specified key in this map.
-- If no hash collision happened, then increments size and associate key with value.
-- If hash collision happened and no key associated value was found, then associate key with value.
-- Otherwise, replace associated key with value.
putCHT :: (MonadConc m, Eq k, Hashable k) => k                   -- `k` argument key associativity to put.
                                              -> v               -- `v` argument value to put.
                                              -> CHT (STM m) k v -- `CHT (STM m) k v` argument map.
                                              -> m ()            -- ^ `()` return value.
putCHT key value cht = atomically (do
    sz <- readTVar (chtSize cht)
    cap <- capacityCHT cht
    let idx = hashCHT key cap
    buckets <- readTVar (chtBuckets cht)
    collided <- readArray buckets idx
    case collided of
      [] -> do
        writeArray buckets idx [(key, value)]
        writeTVar (chtSize cht) (sz + 1)
        resizeCHT cht
      lst -> case lookup key lst of
              Nothing -> do
                let newCollided = (key, value) : collided
                writeArray buckets idx newCollided
              Just _  -> do
                let newCollided = map (\pair@(keyOld , _) -> if keyOld == key then (key, value) else pair) collided
                writeArray buckets idx newCollided
  )

-- | Returns the number of non hash-collision key-value mappings in this map.
sizeCHT :: MonadConc m => CHT (STM m) k v -- ^ `CHT (STM m) k v` argument map.
                          -> m Int        -- ^ `Int` return value.
sizeCHT cht = atomically (do
    readTVar (chtSize cht)
  )

-- | Return the number of preallocated elements in this map.
capacityCHT :: MonadSTM m => CHT m k v -- ^ `CHT m k v` argument map.
                              -> m Int -- ^ `Int` return value.
capacityCHT cht = do
  buckets <- readTVar (chtBuckets cht)
  (_, lastIdx) <- getBounds buckets
  return (lastIdx + 1)

-- | Resizes the preallocated storage in this map.
-- If current `sizeCHT` is less than "capacity * loadFactor", then do nothing.
-- Otherwise, preserve "2 * capacity" as preallocated elements in map.
resizeCHT :: (MonadSTM m, Hashable k) => CHT m k v -- ^ `CHT m k v` argument map.
                                      -> m ()      -- ^ `()` return value.
resizeCHT cht = do
  sz <- readTVar (chtSize cht)
  cap <- capacityCHT cht
  when (fromIntegral sz >= fromIntegral cap * loadFactor) (do
    let newCap = cap * 2
    newInitializedStorage <- newArray (0, newCap - 1) []
    oldBuckets <- readTVar (chtBuckets cht)
    rawBuckets <- getElems oldBuckets
    mapM_ (\x -> do
              mapM_ (\el@(yk, _) -> do
                    let newI = hashCHT yk newCap
                    ell <- readArray newInitializedStorage newI
                    let cat = el : ell
                    writeArray newInitializedStorage newI cat
                ) x
      ) rawBuckets
    writeTVar (chtBuckets cht) newInitializedStorage)
