module HW2.T3
  ( epart,
    mcat,
  )
where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- Helper as apply-function for `foldr` in `mcat`.
-- Monoid's friendly law apply: if it's Nothing, then return this Monoid (as accumulator)
-- Otherwise: apply (<>) in right order.
mcatImpl :: Monoid a => Maybe a -> a -> a
mcatImpl Nothing lst    = lst
mcatImpl (Just cur) lst = cur <> lst

-- Main implementation of `mcat`, using `foldr`.
mcat :: Monoid a => [Maybe a] -> a
mcat = foldr mcatImpl mempty

-- Helper as apply-function for `foldr` in `epart`.
-- If its Left, then apply (<>) to left object in right order and don't touch right object.
-- Otherwise: apply (<>) to right object in right order and don't touch left object.
epartImpl :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
epartImpl (Left cur) (lstf, lsts)  = (cur <> lstf, lsts)
epartImpl (Right cur) (lstf, lsts) = (lstf, cur <> lsts)

-- Main implementation of `epart`, using `foldr`.
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr epartImpl (mempty, mempty)
