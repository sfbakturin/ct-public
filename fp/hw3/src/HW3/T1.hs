module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data Option a = None | Some a
  deriving (Show, Eq)

-- | Main implementation of `mapOption` from Option A to Option B.
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None        = None
mapOption f (Some left) = Some (f left)

data Pair a = P a a
  deriving (Show, Eq)

-- | Main implementation of `mapPair` from Pair A A to Pair B B.
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P left right) = P (f left) (f right)

data Quad a = Q a a a a
  deriving (Show, Eq)

-- | Main implementation of `mapQuad` from Quad A A A A to Quad B B B B.
mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q first second third fourth) = Q (f first) (f second) (f third) (f fourth)

data Annotated e a = a :# e
  deriving (Show, Eq)

infix 0 :#

-- | Main implementation of `mapAnnotated` rebuilding only left value.
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (left :# right) = f left :# right

data Except e a = Error e | Success a
  deriving (Show, Eq)

-- | Main implementation of `mapExcept` rebuilding successfully value with function.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error left)   = Error left
mapExcept f (Success left) = Success (f left)

data Prioritised a = Low a | Medium a | High a
  deriving (Show, Eq)

-- | Main implementation of `mapPrioritised` rebuilding value with saved priority.
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low left)    = Low (f left)
mapPrioritised f (Medium left) = Medium (f left)
mapPrioritised f (High left)   = High (f left)

data Stream a = a :> Stream a
  deriving Show

infixr 5 :>

-- | Main implementation of `mapStream` rebuilding infinity stream value.
mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (left :> right) = f left :> mapStream f right

data List a = Nil | a :. List a
  deriving (Show, Eq)

infixr 5 :.

-- | Main implementation of `mapList` rebuilding all values from list.
mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil             = Nil
mapList f (left :. right) = f left :. mapList f right

data Fun i a = F (i -> a)

-- | Main implementation of `mapFun` rebuilding to composition of two functions.
mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F left) = F (f . left)

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving (Show, Eq)

-- | Main implementation of `mapTree` rebuilding all values of tree with function.
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf                     = Leaf
mapTree f (Branch left root right) = Branch (mapTree f left) (f root) (mapTree f right)
