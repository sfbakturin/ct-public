module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Set (fromList)
import HW5.Action (HIO (..), HiPermission (..))
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

hiRepl :: InputT IO ()
hiRepl = do
  user <- getInputLine "hi> "
  case user of
    Nothing -> outputStrLn "Empty input."
    Just "quit" -> return ()
    Just "exit" -> return ()
    Just ":q" -> return ()
    Just expr -> do
      let parsed = parse expr
      case parsed of
        Left parsedFailed -> outputStrLn (show parsedFailed)
        Right parsedSuccess -> do
          evaluated <- liftIO (runHIO (eval parsedSuccess) (fromList [AllowRead, AllowWrite, AllowTime]))
          case evaluated of
            Left evaluatedError    -> outputStrLn (show evaluatedError)
            Right evaluatedSuccess -> outputStrLn (show (prettyValue evaluatedSuccess))
      hiRepl

main :: IO ()
main = runInputT defaultSettings hiRepl
