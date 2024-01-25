-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , gWrite
  , gGoUp
  , gGoDown
  , gGoLeft
  , gGoRight
  , gToString
  , gNeighbors
  ) where

import Control.Comonad (Comonad (..))

import Control.Monad (liftM2)
import Data.ListZipper (ListZipper (..), lzGenerator, lzGoLeft, lzGoRight, lzToList, lzToString,
                        lzWrite)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

-- | Rewrites focused value of this `Grid`.
gWrite :: a -> Grid a -> Grid a
gWrite x (Grid g) = Grid (lzWrite (lzWrite x (extract g)) g)

-- | Returns an list containing all of the `ListZipper` in this `Grid`.
gToLZList :: Int -> Int -> Grid a -> [ListZipper a]
gToLZList ln rn (Grid g) = lzToList ln rn g

-- | Returns a `String` representation of this `Grid`.
-- The string representation consists of a grid limited by given sizes.
gToString :: Int -> Int -> (a -> String) -> Grid a -> String
gToString ln rn toStr g = unlines (map lzsToStr lzs)
  where
    lzs = gToLZList ln rn g
    lzsToStr = lzToString ln rn toStr

-- | Returns a "go up" of this `Grid`.
gGoUp :: Grid a -> Grid a
gGoUp (Grid g) = Grid (lzGoLeft g)

-- | Returns a "go down" of this `Grid`.
gGoDown :: Grid a -> Grid a
gGoDown (Grid g) = Grid (lzGoRight g)

-- | Returns a "go left" of this `Grid`.
gGoLeft :: Grid a -> Grid a
gGoLeft (Grid g) = Grid (fmap lzGoLeft g)

-- | Returns a "go right" of this `Grid`.
gGoRight :: Grid a -> Grid a
gGoRight (Grid g) = Grid (fmap lzGoRight g)

-- | Returns a horizontal elements of this `Grid` as `ListZipper`.
gHorizontal :: Grid a -> ListZipper (Grid a)
gHorizontal = lzGenerator gGoLeft gGoRight

-- | Returns a vertical elements of this `Grid` as `ListZipper`.
gVertical :: Grid a -> ListZipper (Grid a)
gVertical = lzGenerator gGoUp gGoDown

-- | Returns neighbors from focused point of `Grid`.
gNeighbors :: [Grid a -> Grid a]
gNeighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where
    horizontals = [gGoLeft, gGoRight]
    verticals   = [gGoUp, gGoDown]

instance Functor Grid where
  fmap f (Grid g) = Grid (fmap (fmap f) g)

instance Comonad Grid where
  extract = extract . extract . unGrid
  duplicate = Grid . fmap gHorizontal . gVertical
  extend f g = fmap f (duplicate g)
