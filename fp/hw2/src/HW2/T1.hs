module HW2.T1
  ( Tree (..),
    tfoldr,
  )
where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

-- Main implementation of `tfoldr`, using idea from `foldr` and Tree's order.
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ s Leaf                        = s
-- `tfoldr`: prevents unwanted 2 useless calls for Leaf.
tfoldr fn s (Branch _ Leaf root Leaf)  = fn root s
tfoldr fn s (Branch _ left root right) = tfoldr fn (fn root (tfoldr fn s right)) left
