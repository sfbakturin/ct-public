module HW2.T2
  ( joinWith,
    splitOn,
  )
where

import Data.List.NonEmpty

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- Helper as apply-function for `foldr` in `splitOn`.
-- If its separator object `a`, then create and concatenate with accumulator empty "current" List.
-- Otherwise: concatenate to last modified List from `NonEmpty`.
splitOnImpl :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
splitOnImpl sep cur lst
  | sep == cur = ([] :| []) <> lst
  | otherwise = (cur : Data.List.NonEmpty.head lst) :| Data.List.NonEmpty.tail lst

-- Main implementation of `splitOn`, using `foldr`.
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr (splitOnImpl sep) ([] :| [])

-- Helper as apply-function for `foldl` in `joinWith`.
-- Concatenates current object `a` with separator and accumulator.
joinWithImpl :: a -> [a] -> [a] -> [a]
joinWithImpl jnw lst cur = lst ++ [jnw] ++ cur

-- Main implementation of `joinWith`, using `foldl`.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith jnw (b :| e) = b ++ foldl (joinWithImpl jnw) [] e
