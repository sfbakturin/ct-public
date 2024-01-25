module Main (main) where

import HW6.T3 (Config (Config), simulate, view)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper,
                            info, long, metavar, option, progDesc, value, (<**>))
import System.Exit (exitFailure)
import System.Random (getStdGen)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data Flags = Flags
  { prob  :: Double
  , incub :: Int
  , ill   :: Int
  , immun :: Int
  , gsize :: Int
  , maxit :: Int }

flags :: Parser Flags
flags = Flags
        <$> option auto
          ( long "prob" <> metavar "PROBABILITY" <> help "Infection probability" <> value 0.2 )
        <*> option auto
          ( long "incub" <> metavar "INT" <> help "Incubation period duration" <> value 2 )
        <*> option auto
          ( long "ill" <> metavar "INT" <> help "Illness duration" <> value 5 )
        <*> option auto
          ( long "immun" <> metavar "INT" <> help "Immunity duration" <> value 7 )
        <*> option auto
          ( long "grid-size" <> metavar "INT" <> help "Output grid size" <> value 11 )
        <*> option auto
          ( long "iterations" <> metavar "INT" <> help "The number of simulation iterations"<> value 10 )

opts :: ParserInfo Flags
opts = info (flags <**> helper)
  ( fullDesc
  <> progDesc "Simulate a Covid-19 infection"
  <> header "comonad19 - a simulation of Covid-19 infection on a 2-dimensional grid" )

checkProbability :: Flags -> Maybe String
checkProbability (Flags probability _ _ _ _ _)
  | probability < 0 || probability > 1
    = Just "Probability should be in [0, 1]."
  | otherwise = Nothing

checkAboveZero :: Flags -> Maybe String
checkAboveZero (Flags _ incubation illness immunity gridSize iterations)
  | any (<0) [incubation, illness, immunity, gridSize, iterations]
    = Just "All options must be above zero."
  | otherwise = Nothing

check :: Flags -> IO ()
check fs = do
  let checks = [checkProbability , checkAboveZero]
  mapM_ (\x -> case x fs of
              Nothing  -> return ()
              Just msg -> putStrLn msg >> exitFailure
    ) checks

main :: IO ()
main = do
  initGen <- getStdGen
  rawConfigs <- execParser opts

  check rawConfigs

  let probability = prob rawConfigs
      incubation  = incub rawConfigs
      illness     = ill rawConfigs
      immunity    = immun rawConfigs
      gridSize    = gsize rawConfigs
      iterations  = maxit rawConfigs

  let cfg = Config probability incubation illness immunity
      gens = take iterations (simulate cfg initGen)
      leftN = div (gridSize - 1) 2
      rightN = div gridSize 2

  mapM_ (putStrLn . view leftN rightN) gens
