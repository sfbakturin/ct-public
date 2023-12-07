module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- | Main implementation of `joinOption` returning value from deep of only value.
joinOption :: Option (Option a) -> Option a
joinOption None        = None
joinOption (Some left) = left

-- | Main implementation of `joinExcept` returning value from deep only if it's success only.
joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)      = Error e
joinExcept (Success left) = left

-- | Main implementation of `joinAnnotated` returning value from deep concating right value.
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((lleft :# lright) :# right) = lleft :# (right <> lright)

-- | Main implementation of `joinList` returning concated value of List of Lists.
joinList :: List (List a) -> List a
joinList Nil               = Nil
joinList (Nil :. l)        = joinList l
joinList ((hh :. ll) :. l) = hh :. joinList (ll :. l)

-- | Main implementation of `joinFun` returning re-opened twice function to get value and construct it to Fun.
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F left) = F (\ignore -> let (F f) = left ignore in f ignore)
