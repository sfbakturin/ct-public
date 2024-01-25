module HW5.EvaluatorCommon
  (
    evalRangeFromTo,
    zlibBestCompression,
    firstError,
    getIntegerFromRatio,
    getSubstringFromText,
    getSubsequenceFromSeq,
    getSubbytesFromByteString,
    getIntListFromBytes,
    getBytesFromList,
    getWord8FromRational,
    getNormalFromDiff,
    maybeRatToInt,
    splitAllChars,
    countFromArray,
    selectAssocDict
  ) where

import Codec.Compression.Zlib (CompressParams, bestCompression, compressLevel,
                               defaultCompressParams)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Ratio (denominator)
import Data.Scientific (floatingOrInteger, fromRationalRepetendUnlimited)
import Data.Sequence (Seq (Empty, (:<|)), drop, take)
import Data.Text (Text, drop, length, singleton, take, unpack)
import Data.Time (NominalDiffTime, addUTCTime)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import GHC.Real (numerator)
import HW5.Base (HiError (..), HiValue (..))

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- | Utility function to generate range from-to.
-- If left less-than right, than continue to generate
-- If left equals right, than return singleton
-- Otherwise, return empty
evalRangeFromTo :: Rational  -- ^ `Rational` argument of range begin
                -> Rational  -- ^ `Rational` argument of range end
                -> [HiValue] -- ^ `[HiValue]` return value
evalRangeFromTo l r
  | l < r = HiValueNumber l : evalRangeFromTo (l + 1) r
  | l == r = [HiValueNumber l]
  | otherwise = []

-- | Utility function to generate best level compression for ZLIB library.
zlibBestCompression :: CompressParams -- ^ `CompressParams` return value
zlibBestCompression = defaultCompressParams { compressLevel = bestCompression }

-- | Utility function to return first error from list.
-- If no error found, then failed with error message
firstError :: [Either HiError HiValue] -- ^ `[Either HiError HiValue]` argument to return first error
            -> HiError                 -- ^ `HiError` return value
firstError [] = error "No error value found in the list."
firstError (x:xs) = case x of
    Left err -> err
    Right _  -> firstError xs

-- | Utility function to get integer part from `Rational` only if its only integer.
-- If its only integer, then return just value with `Integer`
-- Otherwise, return `Nothing` as failed
getIntegerFromRatio :: Rational      -- ^ `Rational` argument to get only integer part of
                    -> Maybe Integer -- ^ `Maybe Integer` return value
getIntegerFromRatio r =
  let (n, idx) = fromRationalRepetendUnlimited r
    in case idx of
      Nothing -> case (floatingOrInteger n :: Either Double Integer) of
                  Left _  -> Nothing
                  Right i -> Just i
      Just _ -> Nothing

-- | Utility function to get substring part from string as `Text`.
-- If start >= 0 and end >= 0, return substring
-- Otherwise, adjust indexes to non-negative and return substrings
getSubstringFromText :: Int   -- ^ `Int` argument of start index for slicing
                      -> Int  -- ^ `Int` argument of end index for slicing
                      -> Text -- ^ `Text` argument to slice substring
                      -> Text -- ^ `Text` return value
getSubstringFromText start end str
  | start >= 0 && end >= 0 = (Data.Text.take (end - start) . Data.Text.drop start) str
  | start < 0 && end >= 0 = do
    let idxStart = Data.Text.length str + start
    let realStart = max start idxStart
    (Data.Text.take (end - realStart) . Data.Text.drop realStart) str
  | start >= 0 && end < 0 = do
    let idxEnd = Data.Text.length str + end
    let realEnd = max end idxEnd
    (Data.Text.take (realEnd - start) . Data.Text.drop start) str
  | otherwise = do
    let (idxStart, idxEnd) = (Data.Text.length str + start, Data.Text.length str + end)
    let (realStart, realEnd) = (max start idxStart, max end idxEnd)
    (Data.Text.take (realEnd - realStart) . Data.Text.drop realStart) str

-- | Utility function to get sublist part from list as `Seq`.
getSubsequenceFromSeq :: Int   -- ^ `Int` argument of start index for sublist
                      -> Int   -- ^ `Int` argument of end index for sublist
                      -> Seq a -- ^ `Seq a` argument to slice sublist
                      -> Seq a -- ^ `Seq a` return value
getSubsequenceFromSeq start end = Data.Sequence.take (end - start) . Data.Sequence.drop start

-- | Utility function to get subbytes part from list as `ByteString`.
getSubbytesFromByteString :: Int        -- ^ `Int` argument of start index for subbytes
                          -> Int        -- ^ `Int` argument of end index for subbytes
                          -> ByteString -- ^ `ByteString` argument to slice subbytes
                          -> ByteString -- ^ `ByteString` return value
getSubbytesFromByteString start end = Data.ByteString.take (end - start) . Data.ByteString.drop start

-- | Utility function to map all bytes to `Rational` type.
getIntListFromBytes :: [Word8]    -- ^ `[Word8]` argument to map
                    -> [Rational] -- ^ `[Rational]` return value
getIntListFromBytes = map fromIntegral

-- | Utility function to collect list to byte array.
-- If all numbers are in range of [0, 255], return byte array
-- Otherwise, return invalid argument error
getBytesFromList :: Seq HiValue             -- ^ `Seq HiValue` argument to collect into byte array
                  -> Either HiError [Word8] -- ^ `Either HiError [Word8]` return value
getBytesFromList Data.Sequence.Empty = Right []
getBytesFromList ((HiValueNumber left) :<| lstTail) =
  let l = getWord8FromRational left
    in case l of
      Nothing -> Left HiErrorInvalidArgument
      Just wl -> let rest = getBytesFromList lstTail
                  in case rest of
                    Left err -> Left err
                    Right bs -> Right (wl : bs)
getBytesFromList _ = Left HiErrorInvalidArgument

-- | Utility function to get byte part from `Rational` only if its only byte.
-- If its only integer, then return just value with `Word8`
-- Otherwise, return `Nothing` as failed
getWord8FromRational :: Rational
                      -> Maybe Word8
getWord8FromRational r
  | r >= 0 && r <= 255 = let i = getIntegerFromRatio r
                                                        in case i of
                                                          Nothing -> Nothing
                                                          Just c  -> Just (toEnum (fromIntegral c))
  | otherwise = Nothing

-- | Utility function to convert from `NominalDiffTime` to normal `UTCTime`.
getNormalFromDiff :: NominalDiffTime -- ^ `NominalDiffTime` argument to convert
                  -> UTCTime         -- ^ `UTCTime` return value
getNormalFromDiff diffTime = addUTCTime diffTime (read "1970-01-01 00:00:00 UTC")

-- | Utility function to safely convert from `Rational` to only `Int` type.
-- If its a real integer type, then return integer part
-- Otherwise, fails as `Nothing`
maybeRatToInt :: Rational -> Maybe Int
maybeRatToInt r =
  if denominator r == 1
  then Just (fromIntegral (numerator r))
  else Nothing

splitAllChars :: Text -> [Text]
splitAllChars txt = map singleton (unpack txt)

countFromArray :: Eq a => a -> [a] -> Int
countFromArray x xs = Prelude.length (filter (== x) xs)

selectAssocDict :: Eq a => a -> [(a, b)] -> [b]
selectAssocDict a = map snd . filter (\(x, _) -> x == a)
