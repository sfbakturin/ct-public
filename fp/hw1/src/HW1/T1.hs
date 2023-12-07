module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

afterDays :: Natural -> Day -> Day
afterDays 0 y = y
afterDays 1 y = nextDay y
afterDays x y = afterDays (x - 1) (nextDay y)

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Day -> Natural
daysToParty Monday    = 4
daysToParty Tuesday   = 3
daysToParty Wednesday = 2
daysToParty Thursday  = 1
daysToParty Friday    = 0
daysToParty Saturday  = 6
daysToParty Sunday    = 5
