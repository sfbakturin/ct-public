{-# LANGUAGE DerivingVia #-}
module HW5.Action
  (
    HiPermission(..)
    , PermissionException(..)
    , HIO(..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Reader (ReaderT (..))
import Data.ByteString (ByteString, readFile, writeFile)
import Data.Sequence (Seq, fromList)
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import HW5.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random (randomRIO)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via ReaderT (Set HiPermission) IO

instance HiMonad HIO where
  runAction (HiActionRead pathTo)      = runFRead pathTo
  runAction (HiActionWrite pathTo str) = runFWrite pathTo str
  runAction (HiActionMkDir pathTo)     = runFMkDir pathTo
  runAction (HiActionChDir pathTo)     = runFChDir pathTo
  runAction HiActionCwd                = runFCwd
  runAction HiActionNow                = runFNow
  runAction (HiActionRand start end)   = runFRand start end
  runAction (HiActionEcho txt)         = runFEcho txt

-- | `HiMonad HIO` runner to read something.
-- If its file, then read and try to decode as UTF8, if successful, then return as `String`, else return as byte-sequence
-- Otherwise, get list of files in current directory
runFRead :: FilePath     -- ^ `FilePath` argument
          -> HIO HiValue -- ^ `HIO HiValue` return value
runFRead pathTo = HIO (\permissions -> if AllowRead `elem` permissions
                                        then do
                                          isDir <- doesDirectoryExist pathTo
                                          if isDir
                                          then do
                                            ls <- getListOfElements pathTo
                                            vals <- getSeqFrom ls
                                            return (HiValueList vals)
                                          else do
                                            buf <- Data.ByteString.readFile pathTo
                                            case decodeUtf8' buf of
                                              Left _    -> return (HiValueBytes buf)
                                              Right txt -> return (HiValueString txt)
                                        else (throwIO . PermissionRequired) AllowRead
  )

-- | Helper function to get output of listing directories.
getListOfElements :: FilePath  -- ^ `FilePath` argument to list
                  -> IO [Text] -- ^ `IO [Text]` return value
getListOfElements ioFilePath = do
    fileNames <- listDirectory ioFilePath
    return (map pack fileNames)

-- | Helper function converting list of names to `HiValueList`.
getSeqFrom :: [Text]            -- ^ `[Text]` argument converting
            -> IO (Seq HiValue) -- ^ `IO (Seq HiValue)` return value
getSeqFrom ls = do
  let vals = map (\x -> HiValueString x) ls
  return (fromList vals)

-- | `HiMonad HIO` runner to write bytes to specified path.
runFWrite :: FilePath    -- ^ `FilePath` argument path to write
          -> ByteString  -- ^ `ByteString` argument to write
          -> HIO HiValue -- ^ `HIO HiValue` return value
runFWrite pathTo bs = HIO (\permissions -> if AllowWrite `elem` permissions
                                        then do
                                          _ <- Data.ByteString.writeFile pathTo bs
                                          return HiValueNull
                                        else (throwIO . PermissionRequired) AllowWrite
  )

-- | `HiMonad HIO` runner to create directory with name.
runFMkDir :: FilePath    -- ^ `FilePath` argument path to create
          -> HIO HiValue -- ^ `HIO HiValue` return value
runFMkDir pathTo = HIO (\permissions -> if AllowWrite `elem` permissions
                                        then do
                                          _ <- createDirectory pathTo
                                          return HiValueNull
                                        else (throwIO . PermissionRequired) AllowWrite
  )

-- | `HiMonad HIO` runner to get current system time.
runFChDir :: FilePath    -- ^ `FilePath` argument path to change
          -> HIO HiValue -- ^ `HIO HiValue` return value
runFChDir pathTo = HIO (\permissions -> if AllowRead `elem` permissions
                                      then do
                                        _ <- setCurrentDirectory pathTo
                                        return HiValueNull
                                      else (throwIO . PermissionRequired) AllowRead
  )

-- | `HiMonad HIO` runner to get current working directory.
runFCwd :: HIO HiValue -- ^ `HIO HiValue` return value
runFCwd = HIO (\permissions -> if AllowRead `elem` permissions
                                then HiValueString . Data.Text.pack <$> getCurrentDirectory
                                else (throwIO . PermissionRequired) AllowRead
  )

-- | `HiMonad HIO` runner to get current system time.
runFNow :: HIO HiValue -- ^ `HIO HiValue` return value
runFNow = HIO (\permissions -> if AllowTime `elem` permissions
                                then HiValueTime <$> getCurrentTime
                                else (throwIO . PermissionRequired) AllowTime
  )

-- | `HiMonad HIO` runner to get pseudo-random number.
runFRand :: Int          -- ^ `Int` argument of start range
          -> Int         -- ^ `Int` argument of end range
          -> HIO HiValue -- ^ `HIO HiValue` return value
runFRand start end = HIO (\_ -> HiValueNumber . toRational <$> (randomRIO (start, end))
  )

-- | `HiMonad HIO` runner to echo something on stdout.
runFEcho :: Text         -- ^ `Text` argument to print
          -> HIO HiValue -- ^ `HIO HiValue` return value
runFEcho txt = HIO (\permissions -> if AllowWrite `elem` permissions
                                    then do
                                      _ <- print txt
                                      return HiValueNull
                                    else (throwIO . PermissionRequired) AllowWrite
  )
