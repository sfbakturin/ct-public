module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

import Numeric.Natural

data N = Z | S N

nRawDec :: N -> N
nRawDec Z       = Z
nRawDec (S acc) = acc

nRawInc :: N -> N
nRawInc = S

nplus :: N -> N -> N
nplus Z Z        = Z
nplus Z right    = right
nplus left Z     = left
nplus left right = nplus (nRawInc left) (nRawDec right)

nmultImpl :: N -> N -> N -> N
nmultImpl left Z _   = left
nmultImpl left cnt m = nmultImpl (nplus left m) (nRawDec cnt) m

nmult :: N -> N -> N
nmult Z Z        = Z
nmult Z _        = Z
nmult _ Z        = Z
nmult left right = nmultImpl left (nRawDec right) left

nsubChecked :: N -> N -> N
nsubChecked left Z     = left
nsubChecked left right = nsubChecked (nRawDec left) (nRawDec right)

nsub :: N -> N -> Maybe N
nsub Z Z        = Just Z
nsub left Z     = Just left
nsub Z _        = Nothing
nsub left right = nsub (nRawDec left) (nRawDec right)

isNotPresented :: Maybe a -> Bool
isNotPresented (Just _) = False
isNotPresented _        = True

ncmp :: N -> N -> Ordering
ncmp left right
  | isNotPresented (nsub left right) = LT
  | isNotPresented (nsub right left) = GT
  | otherwise = EQ

nFromNaturalImpl :: Natural -> N -> N
nFromNaturalImpl 0 acc    = acc
nFromNaturalImpl base acc = nFromNaturalImpl (base - 1) (S acc)

nFromNatural :: Natural -> N
nFromNatural base = nFromNaturalImpl base Z

nToNumImpl :: Num a => N -> a -> a
nToNumImpl Z acc    = acc
nToNumImpl base acc = nToNumImpl (nRawDec base) (acc + 1)

nToNum :: Num a => N -> a
nToNum Z    = 0
nToNum base = nToNumImpl base 0

nEvenOddImpl :: N -> N -> Bool -> Bool
nEvenOddImpl base acc flag = if ncmp base acc == EQ
                             then flag
                             else nEvenOddImpl base (nRawInc acc) (not flag)

nEven :: N -> Bool
nEven base = nEvenOddImpl base Z True

nOdd :: N -> Bool
nOdd base = nEvenOddImpl base Z False

ndivImpl :: N -> N -> N -> N
ndivImpl base d acc = if ncmp base d == LT
                      then acc
                      else ndivImpl (nsubChecked base d) d (nRawInc acc)

ndiv :: N -> N -> N
ndiv left right = if ncmp left right == LT
                  then Z
                  else ndivImpl left right Z

nmod :: N -> N -> N
nmod left right = nsubChecked left (nmult (ndiv left right) right)
