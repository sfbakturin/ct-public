module HW0.T3
  ( compose
  , contract
  , i
  , k
  , permute
  , s
  ) where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- S
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g argA = f argA (g argA)

-- K
k :: a -> b -> a
k argA _ = argA

-- I
i :: a -> a
i = s k k

-- B
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

-- W
contract :: (a -> a -> b) -> (a -> b)
contract = s s (k (s k k))

-- C
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k s) k (s (k s) k) s) (k k)
