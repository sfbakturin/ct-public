module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import Data.Char (isSpace)
import HW0.T1 (distrib)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

a = distrib (Left ("AB" ++ "CD" ++ "EF"))

a_whnf = (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))

b = map isSpace "Hello, World"

b_whnf = False : map isSpace "ello, World"

c = if 1 > 0 || error "X" then "Y" else "Z"

c_whnf = "Y"
