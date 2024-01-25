-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  , lzWrite
  , lzGoLeft
  , lzGoRight
  , lzGenerator
  , lzToList
  , lzToString
  ) where

import Control.Comonad (Comonad (..))

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data ListZipper a = LZ [a] a [a]

-- | Rewrites focused value of this `ListZipper`.
lzWrite :: a -> ListZipper a -> ListZipper a
lzWrite newFocus (LZ left _ right) = LZ left newFocus right

-- | Returns an list containing all of the elements in this `ListZipper`.
lzToList :: Int -> Int -> ListZipper a -> [a]
lzToList ln rn (LZ left focus right) = pLeft ++ pFocus ++ pRight
  where
    pLeft = reverse (take ln left)
    pRight = take rn right
    pFocus = [focus]

-- | Returns a `String` representation of this `ListZipper`.
-- The string representation consists of a list-zipper limited by given sizes.
lzToString :: Int -> Int -> (a -> String) -> ListZipper a -> String
lzToString ln rn toStr lz = concatMap toStr array
  where
    array = lzToList ln rn lz

-- | Returns a "go left" of this `ListZipper`.
lzGoLeft :: ListZipper a -> ListZipper a
lzGoLeft (LZ (leftHead : leftRest) focus right) = LZ leftRest leftHead (focus : right)
lzGoLeft lz                                     = lz

-- | Returns a "go right" of this `ListZipper`.
lzGoRight :: ListZipper a -> ListZipper a
lzGoRight (LZ left focus (rightHead : rightRest)) = LZ (focus : left) rightHead rightRest
lzGoRight lz                                      =  lz

-- | Helper function to infinitely iterate and use given function.
iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

-- | Returns infinite generated `ListZipper` with given function and init focused value.
lzGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lzGenerator f g focus = LZ (iterateTail f focus) focus (iterateTail g focus)

instance Functor ListZipper where
  fmap f (LZ left focus right) = LZ (map f left) (f focus) (map f right)

instance Comonad ListZipper where
  extract (LZ _ focus _) = focus
  duplicate = lzGenerator lzGoLeft lzGoRight
  extend f lz = fmap f (duplicate lz)
