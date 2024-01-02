module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Control.Monad.Fix (fix)
import Numeric.Natural (Natural)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- | Another implementation of `Data.List.repeat` using `fix`.
repeat' :: a       -- ^ `a` argument to make singleton
            -> [a] -- ^ `[a]` return value
repeat' e = fix (e:)

-- | Another implementation of `Data.List.map` using `fix`.
map' :: (a -> b) -- ^ `(a -> b)` argument for mapping
        -> [a]   -- ^ `[a]` argument of in-arguments
        -> [b]   -- ^ `[b]` return value
map' = fix (\f g l -> case l of
                      []    -> []
                      (h:t) -> g h : f g t
  )

-- | Helper function of implementation recursive using `fix`.
fibImpl :: Natural     -- ^ `Natural` argument of previous state
            -> Natural -- ^ `Natural` argument of current state (accumulator)
            -> Natural -- ^ `Natural` argument of current N
            -> Natural -- ^ `Natural` return value
fibImpl = fix (\f prev acc n -> if n <= 0 then acc else f acc (prev + acc) (n - 1))

-- | Gets Nth Fibonacci number using `fix`.
fib :: Natural     -- ^ `Natural` argument Nth
        -> Natural -- ^ `Natural` return value
fib = fibImpl 1 0

-- | Gets number factorial using `fix`.
fac :: Natural     -- ^ `Natural` argument number to get factorial of
        -> Natural -- ^ `Natural` return value
fac = fix (\f n -> if n <= 1 then 1 else n * f (n - 1))
