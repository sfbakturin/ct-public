module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- | Main implementation of `distOption` returning only value, when it's defined all of them.
distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)               = None
distOption (_, None)               = None
distOption (Some left, Some right) = Some (left, right)

-- | Main implementation of `wrapOption` returning value as defined.
wrapOption :: a -> Option a
wrapOption = Some

-- | Main implementation of `distPair` returning pairs of lefts or rights value.
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P lleft lright, P rleft rright) = P (lleft, rleft) (lright, rright)

-- | Main implementation of `wrapPair` returning pair of left==right values.
wrapPair :: a -> Pair a
wrapPair left = P left left

-- | Main implementation of `distQuad` returning quad of pairs of lefts, rights, second lefts, rights, third lefts and rights and fourth.
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q lfirst ls lt lf, Q rfirst rs rt rf) = Q (lfirst, rfirst) (ls, rs) (lt, rt) (lf, rf)

-- | Main implementation of `wrapQuad` returning quad of first==second==third==fourth values.
wrapQuad :: a -> Quad a
wrapQuad left = Q left left left left

-- | Main implementation of `distAnnotated` returning pairs of only-values and using property of Semigroup - concat rights.
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (lleft :# lright, rleft :# rright) = (lleft, rleft) :# (lright <> rright)

-- | Main implementation of `wrapAnnotated` returning only value using property of Monoid with empty right.
wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated left = left :# mempty

-- | Main implementation of `distExcept` returning success only if only two of them is successfully.
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error left, _)               = Error left
distExcept (_, Error right)              = Error right
distExcept (Success left, Success right) = Success (left, right)

-- | Main implementation of `wrapExcept` returning success only from value.
wrapExcept :: a -> Except e a
wrapExcept = Success

-- | Main implementation of `distPrioritised` returning pair of highest of two priorities.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low left, Low right)       = Low (left, right)
distPrioritised (Low left, Medium right)    = Medium (left, right)
distPrioritised (Medium left, Low right)    = Medium (left, right)
distPrioritised (Medium left, Medium right) = Medium (left, right)
distPrioritised (High left, High right)     = High (left, right)
distPrioritised (High left, Medium right)   = High (left, right)
distPrioritised (Medium left, High right)   = High (left, right)
distPrioritised (Low left, High right)      = High (left, right)
distPrioritised (High left, Low right)      = High (left, right)

-- | Main implementation of `wrapPrioritised` returning low priority.
wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- | Main implementation of `distStream` returning infinity stream of pairs.
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (lleft :> lright, rleft :> rright) = (lleft, rleft) :> distStream (lright, rright)

-- | Main implementation of `wrapStream` returning infinity stream of value.
wrapStream :: a -> Stream a
wrapStream left = left :> wrapStream left

-- | Helper as convert from List to array function for `distList`.
-- | If its Nil, then return accumulator, otherwise -- concatenate in right order value and continue.
listToArrayImpl :: List a -> [a] -> [a]
listToArrayImpl Nil acc             = acc
listToArrayImpl (left :. right) acc = listToArrayImpl right (acc ++ [left])

-- | Helper as convert from List to array function for `distList`.
-- | Used as wrapper over with empty accumulator.
listToArray :: List a -> [a]
listToArray left = listToArrayImpl left []

-- | Helper to dist two arrays.
twoListDistImpl :: a -> [b] -> [(a, b)]
twoListDistImpl _ []       = []
twoListDistImpl cur (x:xs) = (cur, x) : twoListDistImpl cur xs

-- | Helper to dist two arrays.
-- | Used as wrapper over with empty accumulator.
twoListDist :: [a] -> [b] -> [(a, b)]
twoListDist [] _     = []
twoListDist (x:xs) y =  twoListDistImpl x y ++ twoListDist xs y

-- | Helper to convert array of pairs to List.
-- | Used as wrapper over with empty accumulator.
arrayToList :: [a] -> List a
arrayToList []     = Nil
arrayToList (x:xs) = x :. arrayToList xs

-- | Main implementation of `distList` returning pairs of first's values with all of seconds.
distList :: (List a, List b) -> List (a, b)
distList (_, Nil)      = Nil
distList (Nil, _)      = Nil
distList (left, right) = arrayToList (twoListDist (listToArray left) (listToArray right))

-- | Main implementation of `wrapList` returning list only with one element.
wrapList :: a -> List a
wrapList left = left :. Nil

-- | Main implementation of `distFun` returning pair of ignored -> first, second as re-opened of them.
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F left, F right) = F (\ignore -> (left ignore, right ignore))

-- | Main implementation of `wrapFun` returning function that will not use first argument, but return value.
wrapFun :: a -> Fun i a
wrapFun left = F (const left)
