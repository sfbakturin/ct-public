{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data a <-> b = Iso (a -> b) (b -> a)

-- | Make `Either` as distribution `a` over the sum of `b` and `c` into pair type.
-- If its `Left` value assigned, then returns both elements as `Left` value assigned
-- Otherwise, returns both elements as `Right` value assigned
distrib :: Either a (b, c)              -- ^ `Either a (b, c)` argument of distribution to pair type
            -> (Either a b, Either a c) -- ^ `(Either a b, Either a c)` return value
distrib (Left left)           = (Left left, Left left)
distrib (Right (left, right)) = (Right left, Right right)

-- | Make isomorphism to flip switching the directions.
-- Returns as isomorphism right-left elements
flipIso :: (a <-> b)     -- ^ `(a <-> b)` argument to flip
            -> (b <-> a) -- ^ `(b <-> a)` return value
flipIso (Iso left right) = Iso right left

-- | Make extracting the first conversion from isomorphism.
-- Return first argument of isomorphism element
runIso :: (a <-> b)   -- ^ `(a <-> b)` argument to extract
          -> (a -> b) -- ^ `(a -> b)` return value
runIso (Iso left _) = left

-- | Make isomorphism between pairs `(a, (b, c))` and `((a, b), c)`.
assocPair :: (a, (b, c)) <-> ((a, b), c) -- ^ `(a, (b, c)) <-> ((a, b), c)` return value
assocPair = Iso (\(left, (med, right)) -> ((left, med), right)) (\((left, med), right) -> (left, (med, right)))

-- | Make isomorphism between `Either`s of types `a`, `b`-`c` and `a`-`b`, `c`.
assocEither :: Either a (Either b c) <-> Either (Either a b) c -- ^ `Either a (Either b c) <-> Either (Either a b) c` return value
assocEither = Iso assocEitherImplLeft assocEitherImplRight

-- | Implementation helper for `Left` value assigned for `assocEither`.
-- If its `Left` value assigned (type `a`), then returns as left's `Left` value
-- If its `Right` value assigned and its `Left` (type `b`), then returns as left's `Right` value
-- Otherwise, returns as `Right` value
assocEitherImplLeft :: Either a (Either b c)     -- ^ `Either a (Either b c)` argument to re-cover `Either`
                        -> Either (Either a b) c -- ^ `Either (Either a b) c` return value
assocEitherImplLeft (Left left)          = Left (Left left)
assocEitherImplLeft (Right (Left left))  = Left (Right left)
assocEitherImplLeft (Right (Right left)) = Right left

-- | Implementation helper for `Right` value assigned for `assocEither`.
-- If its `Right` value assigned (type `c`), then returns as right's `Right` value
-- If its `Left` value assigned and its `Left` (type `a`), then returns as right's `Left` value
-- Otherwise, returns as `Left` value
assocEitherImplRight :: Either (Either a b) c    -- ^ `Either (Either a b) c` argument to re-cover `Either`
                        -> Either a (Either b c) -- ^ `Either a (Either b c)` return value
assocEitherImplRight (Left (Left right))  = Left right
assocEitherImplRight (Left (Right right)) = Right (Left right)
assocEitherImplRight (Right right)        = Right (Right right)
