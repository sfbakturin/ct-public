{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits (Symbol)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains s (s ': t) = 'True
  Contains s (_ ': t) = Contains s t
  Contains s '[]      = 'False

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete s (s ': t) = t
  Delete s (v ': t) = v ': Delete s t
  Delete s '[]      = '[]

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add s (s ': t) = s ': t
  Add s (v ': t) = v ': Add s t
  Add s '[]      = s ': '[]
