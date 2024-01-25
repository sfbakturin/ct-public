module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  , simulate

  , view
  ) where

import System.Random (Random (randomR, randoms), StdGen, mkStdGen)

import Control.Comonad (Comonad (extend, extract))
import Data.Grid (Grid (..), gNeighbors, gToString, gWrite)
import Data.ListZipper (ListZipper (LZ), lzGenerator)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data Config = Config
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

-- | Returns a `String` representation of this `Cell`.
instance Show Cell where
  show (Cell Healthy _)      = "_"
  show (Cell (Infected _) _) = "i"
  show (Cell (Ill _) _)      = "#"
  show (Cell (Immune _) _)   = "@"

type Comonad19Grid = Grid Cell

-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
--
-- This function may take additional parameters (e.g. initial seed for random).
simulate :: Config             -- ^ `Config` argument session settings.
            -> StdGen          -- ^ `StdGen` argument initialize pseudo-random generator.
            -> [Comonad19Grid] -- ^ `[Comonad19Grid]` return value.
simulate cfg initSeed = iterate (evolve cfg) initializedField
  where
    seeds = take 2 (randoms initSeed)
    seedL = head seeds
    seedR = last seeds
    genL = mkStdGen seedL
    genR = mkStdGen seedR
    lzSeeds = LZ (randoms genL) 0 (randoms genR)
    gSeeds = Grid (lzGenerator (fmap (+ 4)) (fmap (\x -> x - 8)) lzSeeds)
    field  = fmap (Cell Healthy . mkStdGen) gSeeds
    infectedCell = Cell (Infected (incubationPeriod cfg)) initSeed
    initializedField = gWrite infectedCell field

-- | Returns a `String` representation of this `Comonad19Grid`.
-- The string representation consists of a grid limited by given sizes.
view :: Int              -- ^ `Int` argument Y size.
        -> Int           -- ^ `Int` argument X size.
        -> Comonad19Grid -- ^ `Comonad19Grid` argument current step.
        -> String        -- ^ `String` return value.
view ls rs g = gToString ls rs show g ++ "\n"

-- | Return `True` if this `Cell` is infected.
-- Otherwise, returns `False`.
infected :: Cell    -- ^ `Cell` argument to indicate.
            -> Bool -- ^ `Bool` return value.
infected (Cell (Infected _) _) = True
infected _                     = False

-- | Returns the number of `Cell` that infected.
infectedCount :: [Cell]  -- ^ `[Cell]` argument to count of.
                  -> Int -- ^ `Int` return value.
infectedCount = length . filter infected

-- | Returns the number of infected neighbors of focused `Cell`.
infectedNeighbors :: Comonad19Grid -- ^ `Comonad19Grid` argument focused `Cell`.
                      -> Int       -- ^ `Int` return value.
infectedNeighbors g = infectedCount (map (\d -> extract (d g)) gNeighbors)

-- | Returns the new `Cell` as comonad19's rules.
-- If its `Healthy`, returns as itself.
-- If its any `Infected`, `Ill` or `Immune` and life count is not zero, then
-- decrement count and returns with updated count.
-- Otherwise, change its form and set life count.
counter :: Config   -- ^ `Config` argument session settings.
            -> Cell -- ^ `Cell` argument to change from.
            -> Cell -- ^ `Cell` return value.
counter _ (Cell Healthy seed)        = Cell Healthy seed
counter cfg (Cell (Infected 0) seed) = Cell (Ill (illnessDuration cfg)) seed
counter cfg (Cell (Ill 0) seed)      = Cell (Immune (immunityDuration cfg)) seed
counter _ (Cell (Immune 0) seed)     = Cell Healthy seed
counter _ (Cell (Infected cnt) seed) = Cell (Infected (cnt - 1)) seed
counter _ (Cell (Ill cnt) seed)      = Cell (Ill (cnt - 1)) seed
counter _ (Cell (Immune cnt) seed)   = Cell (Immune (cnt - 1)) seed

-- | Tries to infect this `Cell` with its seed as `StdGen`.
-- If successful, then returns new seed as generated from `randomR` as `StdGen`.
-- Otherwise (when number of tries is zero), returns old one and flag.
infect :: StdGen            -- ^ `StdGen` argument current seed.
          -> Double         -- ^ `Double` argument infection probability.
          -> Int            -- ^ `Int` argument number of tries.
          -> (StdGen, Bool) -- ^ `(StdGen, Bool)` return value.
infect seed _ 0 = (seed, False)
infect seed p n = let (generated, newSeed) = randomR (0, 1) seed
                    in if generated <= p then (newSeed, True) else infect newSeed p (n - 1)

-- | Rules the Comonad19 simulation.
-- If its `Healthy`, then tries to infect by any infected neighbors,
-- if successfully, then it's a new infected `Cell` with set incubation period,
-- else it's `Healthy`.
-- Otherwise, return a new `Cell` as `counter` set.
rule :: Config           -- ^ `Config` argument session settings.
        -> Comonad19Grid -- ^ `Comonad19Grid` argument current step.
        -> Cell          -- ^ `Cell` return value.
rule cfg g = case extract g of
              Cell Healthy oldSeed -> do
                let n = infectedNeighbors g
                    p = probability cfg
                    (finalSeed, finalSuccess) = infect oldSeed p n
                if finalSuccess then Cell (Infected (incubationPeriod cfg)) finalSeed
                                else Cell Healthy finalSeed
              cntCell@(Cell _ _) -> counter cfg cntCell

-- | Returns a new simulation's step.
evolve :: Config           -- ^ `Config` argument session settings.
          -> Comonad19Grid -- ^ `Comonad19Grid` argument previous step.
          -> Comonad19Grid -- ^ `Comonad19Grid` return value.
evolve cfg = extend (rule cfg)
