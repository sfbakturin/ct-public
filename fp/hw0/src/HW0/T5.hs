module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

type Nat a = (a -> a) -> a -> a

-- | Numeral zero.
nz :: Nat a
nz _ n = n

-- | Numeral mapping.
ns :: Nat a    -- ^ `Nat a` argument accumulator
      -> Nat a -- ^ `Nat a` return value
ns n mapper acc = let x = mapper acc in n mapper x

-- | Numeral sum of two mappers.
nplus :: Nat a     -- ^ `Nat a` argument of left value
          -> Nat a -- ^ `Nat a` argument of right value
          -> Nat a -- ^ `Nat a` return value
nplus a b acc dec = a acc (b acc dec)

-- | Numeral multiply of two mappers.
nmult :: Nat a     -- ^ `Nat a` argument of left value
          -> Nat a -- ^ `Nat a` argument of right value
          -> Nat a -- ^ `Nat a` return value
nmult a b acc = a (b acc)

-- | Helper function for `nFromNatural`.
-- If numerical value is zero, then return accumulator
-- Otherwise, map accumulator and decrement numerical, continue
nFromNaturalImpl :: Nat a -> Natural -> Nat a
nFromNaturalImpl acc 0 = acc
nFromNaturalImpl acc n = nFromNaturalImpl (ns acc) (n - 1)

-- | Gets numeral mapper from numerical value.
nFromNatural :: Natural -> Nat a
nFromNatural = nFromNaturalImpl nz

-- | Gets numerical value from numeral mapper.
nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
