module HW2.T4
  ( DotString (..),
    Fun (..),
    Inclusive (..),
    ListPlus (..),
  )
where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show, Eq)

infixr 5 :+

-- Semigroup for `ListPlus`: concatenate lists.
instance Semigroup (ListPlus a) where
  (<>) (Last left) right       = left :+ right
  (<>) (lleft :+ lright) right = lleft :+ (lright <> right)

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This left) (That right)                = Both left right
  (<>) (That left) (This right)                = Both right left
  (<>) (This left) (This right)                = This (left <> right)
  (<>) (That left) (That right)                = That (left <> right)
  (<>) (This left) (Both lright rright)        = Both (left <> lright) rright
  (<>) (That left) (Both lright rright)        = Both lright (left <> rright)
  (<>) (Both lleft rleft) (Both lright rright) = Both (lleft <> lright) (rleft <> rright)
  (<>) (Both lleft rleft) (That right)         = Both lleft (rleft <> right)
  (<>) (Both lleft rleft) (This right)         = Both (lleft <> right) rleft

newtype DotString = DS String
  deriving (Show, Eq)

-- Semigroup for `DotString`: concatenate, if it's not a empty string, otherwise: id.
instance Semigroup DotString where
  (<>) (DS left) (DS "")    = DS left
  (<>) (DS "") (DS right)   = DS right
  (<>) (DS left) (DS right) = DS (left ++ "." ++ right)

-- Monoid for `DotString`: empty string as special case for Semigroup.
instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

-- Semigroup for `Fun`: if its f1 <> f2, then it should be as function's composition
instance Semigroup (Fun a) where
  (<>) (F left) (F right) = F (left . right)

-- Monoid for `Fun`: if its empty-monoid, then it should return function as given.
instance Monoid (Fun a) where
  mempty = F id
