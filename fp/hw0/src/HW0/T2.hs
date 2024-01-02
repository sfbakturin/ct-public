module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

type Not a = a -> Void

-- | Make value as double negation (_ -> Void).
doubleNeg :: a ->         -- ^ `a` argument to negate
              Not (Not a) -- ^ `Not (Not a)` return value
doubleNeg left right = right left

-- | Extract from triple negation.
reduceTripleNeg :: Not (Not (Not a)) -- ^ `Not (Not (Not a))` argument to extract from
                    -> Not a         -- ^ `Not a` return value
reduceTripleNeg left right = left (doubleNeg right)
