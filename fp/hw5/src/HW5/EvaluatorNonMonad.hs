module HW5.EvaluatorNonMonad
  (
    evalNoMonadFold
  ) where

import Codec.Compression.Zlib (compressWith, decompress)
import Codec.Serialise (deserialise, serialise)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Semigroup
import Data.Sequence (Seq, empty, fromList, length, lookup, reverse, (><))
import Data.Text (Text, concat, length, pack, reverse, strip, toLower, toUpper, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Text.Encoding as Data.Text
import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiValue (..), isBinFun)
import HW5.EvaluatorCommon (evalRangeFromTo, getBytesFromList, getIntListFromBytes,
                            getIntegerFromRatio, getSubbytesFromByteString, getSubsequenceFromSeq,
                            getSubstringFromText, zlibBestCompression)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

evalNoMonadFold :: HiExpr -> [HiValue] -> Either HiError HiValue
evalNoMonadFold (HiExprValue (HiValueFunction HiFunAdd)) args = evalFAdd args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunSub)) args = evalFSub args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunMul)) args = evalFMul args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunDiv)) args = evalFDiv args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunNot)) args = evalFNot args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunAnd)) args = evalFAnd args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunOr)) args = evalFOr args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunLessThan)) args = evalFLessThan args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunGreaterThan)) args = evalFGreaterThan args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunEquals)) args = evalFEquals args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunNotLessThan)) args = evalFNotLessThan args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunNotGreaterThan)) args = evalFNotGreaterThan args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunNotEquals)) args = evalFNotEquals args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunIf)) args = evalFIf args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunLength)) args = evalFLength args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunToUpper)) args = evalFToUpper args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunToLower)) args = evalFToLower args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunReverse)) args = evalFReverse args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunTrim)) args = evalFTrim args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunList)) args = evalFList args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunRange)) args = evalFRange args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunFold)) args = evalFFold args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunPackBytes)) args = evalFPackBytes args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunUnpackBytes)) args = evalFUnpackBytes args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunEncodeUtf8)) args = evalFEncodeUtf8 args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunDecodeUtf8)) args = evalFDecodeUtf8 args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunZip)) args = evalFZip args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunUnzip)) args = evalFUnzip args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunSerialise)) args = evalFSerialise args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunDeserialise)) args = evalFDeserialise args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunRead)) args =  evalFRead args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunWrite)) args = evalFWrite args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunMkDir)) args = evalFMkDir args
evalNoMonadFold (HiExprValue (HiValueFunction HiFunChDir)) args = evalFChDir args
evalNoMonadFold (HiExprValue (HiValueString s)) args = evalStrSlice s args
evalNoMonadFold (HiExprValue (HiValueList lst)) args = evalListSlice lst args
evalNoMonadFold (HiExprValue (HiValueBytes bs)) args = evalBytesSlice bs args
evalNoMonadFold (HiExprValue _) _ =  Left HiErrorInvalidFunction
evalNoMonadFold _ _ = Left HiErrorInvalidArgument

evalFAdd :: [HiValue] -> Either HiError HiValue
evalFAdd [HiValueNumber left, HiValueNumber right] = Right (HiValueNumber (left + right))
evalFAdd [HiValueString left, HiValueString right] = Right (HiValueString (Data.Text.concat [left, right]))
evalFAdd [HiValueList left, HiValueList right] = Right (HiValueList (left Data.Sequence.>< right))
evalFAdd [HiValueBytes left, HiValueBytes right] = Right (HiValueBytes (left <> right))
evalFAdd [_, _] = Left HiErrorInvalidArgument
evalFAdd _ = Left HiErrorArityMismatch

evalFSub :: [HiValue] -> Either HiError HiValue
evalFSub [HiValueNumber left, HiValueNumber right] = Right (HiValueNumber (left - right))
evalFSub [_, _]                                    = Left HiErrorInvalidArgument
evalFSub _                                         = Left HiErrorArityMismatch

evalFMul :: [HiValue] -> Either HiError HiValue
evalFMul [HiValueNumber left, HiValueNumber right] = Right (HiValueNumber (left * right))
evalFMul [HiValueString left, HiValueNumber right] =
  let i = getIntegerFromRatio right
    in case i of
      Nothing -> Left HiErrorInvalidArgument
      Just c  -> Right (HiValueString (pack (stimes c (unpack left))))
evalFMul [HiValueList left, HiValueNumber right] =
  let i = getIntegerFromRatio right
    in case i of
      Nothing -> Left HiErrorInvalidArgument
      Just c  -> Right (HiValueList (stimes c left))
evalFMul [HiValueBytes left, HiValueNumber right] =
  let i = getIntegerFromRatio right
    in case i of
      Nothing -> Left HiErrorInvalidArgument
      Just c  -> Right (HiValueBytes (stimes c left))
evalFMul [_, _] = Left HiErrorInvalidArgument
evalFMul _ = Left HiErrorArityMismatch

evalFDiv :: [HiValue] -> Either HiError HiValue
evalFDiv [HiValueNumber left, HiValueNumber right]
  | right == 0 = Left HiErrorDivideByZero
  | otherwise = Right (HiValueNumber (left / right))
evalFDiv [HiValueString left, HiValueString right] = Right (HiValueString (Data.Text.concat [left, pack "/", right]))
evalFDiv [_, _] = Left HiErrorInvalidArgument
evalFDiv _ = Left HiErrorArityMismatch

evalFNot :: [HiValue] -> Either HiError HiValue
evalFNot [HiValueBool left] = Right (HiValueBool (not left))
evalFNot [_]                = Left HiErrorInvalidArgument
evalFNot _                  = Left HiErrorArityMismatch

evalFAnd :: [HiValue] -> Either HiError HiValue
evalFAnd [HiValueBool left, HiValueBool right] = Right (HiValueBool (left && right))
evalFAnd [_, _]                                = Left HiErrorInvalidArgument
evalFAnd _                                     = Left HiErrorArityMismatch

evalFOr :: [HiValue] -> Either HiError HiValue
evalFOr [HiValueBool left, HiValueBool right] = Right (HiValueBool (left || right))
evalFOr [_, _]                                = Left HiErrorInvalidArgument
evalFOr _                                     = Left HiErrorArityMismatch

evalFLessThan :: [HiValue] -> Either HiError HiValue
evalFLessThan [HiValueNumber left, HiValueNumber right] = Right (HiValueBool (left < right))
evalFLessThan [HiValueNumber _, HiValueBool _]          = Right (HiValueBool False)
evalFLessThan [HiValueBool _, HiValueNumber _]          = Right (HiValueBool True)
evalFLessThan [HiValueBool left, HiValueBool right]     = Right (HiValueBool (left < right))
evalFLessThan [_, _]                                    = Left HiErrorInvalidArgument
evalFLessThan _                                         = Left HiErrorArityMismatch

evalFGreaterThan :: [HiValue] -> Either HiError HiValue
evalFGreaterThan [HiValueNumber left, HiValueNumber right] = Right (HiValueBool (left > right))
evalFGreaterThan [HiValueNumber _, HiValueBool _]          = Right (HiValueBool True)
evalFGreaterThan [HiValueBool _, HiValueNumber _]          = Right (HiValueBool False)
evalFGreaterThan [HiValueBool left, HiValueBool right]     = Right (HiValueBool (left > right))
evalFGreaterThan [_, _]                                    = Left HiErrorInvalidArgument
evalFGreaterThan _                                         = Left HiErrorArityMismatch

evalFEquals :: [HiValue] -> Either HiError HiValue
evalFEquals [HiValueNumber left, HiValueNumber right]     = Right (HiValueBool (left == right))
evalFEquals [HiValueString left, HiValueString right]     = Right (HiValueBool (left == right))
evalFEquals [HiValueNumber _, HiValueBool _]              = Right (HiValueBool False)
evalFEquals [HiValueBool _, HiValueNumber _]              = Right (HiValueBool False)
evalFEquals [HiValueBool left, HiValueBool right]         = Right (HiValueBool (left == right))
evalFEquals [HiValueFunction left, HiValueFunction right] = Right (HiValueBool (left == right))
evalFEquals [_, _]                                        = Left HiErrorInvalidArgument
evalFEquals _                                             = Left HiErrorArityMismatch

evalFNotLessThan :: [HiValue] -> Either HiError HiValue
evalFNotLessThan [HiValueNumber left, HiValueNumber right] = Right (HiValueBool (left >= right))
evalFNotLessThan [HiValueNumber _, HiValueBool _]          = Right (HiValueBool True)
evalFNotLessThan [HiValueBool _, HiValueNumber _]          = Right (HiValueBool False)
evalFNotLessThan [HiValueBool left, HiValueBool right]     = Right (HiValueBool (left >= right))
evalFNotLessThan [_, _]                                    = Left HiErrorInvalidArgument
evalFNotLessThan _                                         = Left HiErrorArityMismatch

evalFNotGreaterThan :: [HiValue] -> Either HiError HiValue
evalFNotGreaterThan [HiValueNumber left, HiValueNumber right] = Right (HiValueBool (left <= right))
evalFNotGreaterThan [HiValueNumber _, HiValueBool _]          = Right (HiValueBool False)
evalFNotGreaterThan [HiValueBool _, HiValueNumber _]          = Right (HiValueBool True)
evalFNotGreaterThan [HiValueBool left, HiValueBool right]     = Right (HiValueBool (left <= right))
evalFNotGreaterThan [_, _]                                    = Left HiErrorInvalidArgument
evalFNotGreaterThan _                                         = Left HiErrorArityMismatch

evalFNotEquals :: [HiValue] -> Either HiError HiValue
evalFNotEquals [HiValueNumber left, HiValueNumber right]     = Right (HiValueBool (left /= right))
evalFNotEquals [HiValueString left, HiValueString right]     = Right (HiValueBool (left /= right))
evalFNotEquals [HiValueNumber _, HiValueBool _]              = Right (HiValueBool True)
evalFNotEquals [HiValueBool _, HiValueNumber _]              = Right (HiValueBool True)
evalFNotEquals [HiValueBool left, HiValueBool right]         = Right (HiValueBool (left /= right))
evalFNotEquals [HiValueFunction left, HiValueFunction right] = Right (HiValueBool (left /= right))
evalFNotEquals [_, _]                                        = Left HiErrorInvalidArgument
evalFNotEquals _                                             = Left HiErrorArityMismatch

evalFIf :: [HiValue] -> Either HiError HiValue
evalFIf [HiValueBool True, left, _]   = Right left
evalFIf [HiValueBool False, _, right] = Right right
evalFIf [_, _, _]                     = Left HiErrorInvalidArgument
evalFIf _                             = Left HiErrorArityMismatch

evalFLength :: [HiValue] -> Either HiError HiValue
evalFLength [HiValueString s] = Right (HiValueNumber (toRational (Data.Text.length s)))
evalFLength [HiValueList lst] = Right (HiValueNumber (toRational (Data.Sequence.length lst)))
evalFLength [_]               = Left HiErrorInvalidArgument
evalFLength _                 = Left HiErrorArityMismatch

evalFToUpper :: [HiValue] -> Either HiError HiValue
evalFToUpper [HiValueString s] = Right (HiValueString (toUpper s))
evalFToUpper [_]               = Left HiErrorInvalidArgument
evalFToUpper _                 = Left HiErrorArityMismatch

evalFToLower :: [HiValue] -> Either HiError HiValue
evalFToLower [HiValueString s] = Right (HiValueString (toLower s))
evalFToLower [_]               = Left HiErrorInvalidArgument
evalFToLower _                 = Left HiErrorArityMismatch

evalFReverse :: [HiValue] -> Either HiError HiValue
evalFReverse [HiValueString s] = Right (HiValueString (Data.Text.reverse s))
evalFReverse [HiValueList lst] = Right (HiValueList (Data.Sequence.reverse lst))
evalFReverse [_]               = Left HiErrorInvalidArgument
evalFReverse _                 = Left HiErrorArityMismatch

evalFTrim :: [HiValue] -> Either HiError HiValue
evalFTrim [HiValueString s] = Right (HiValueString (strip s))
evalFTrim [_]               = Left HiErrorInvalidArgument
evalFTrim _                 = Left HiErrorArityMismatch

evalFList :: [HiValue] -> Either HiError HiValue
evalFList elements = Right (HiValueList (Data.Sequence.fromList elements))

evalFRange :: [HiValue] -> Either HiError HiValue
evalFRange [HiValueNumber from, HiValueNumber to] = Right (HiValueList (Data.Sequence.fromList (evalRangeFromTo from to)))
evalFRange [_, _] = Left HiErrorInvalidArgument
evalFRange _ = Left HiErrorArityMismatch

evalFFold :: [HiValue] -> Either HiError HiValue
evalFFold [HiValueFunction f, HiValueList lst]
  | isBinFun f = let args = toList lst
                    in case args of
                      []     -> Right (HiValueList Data.Sequence.empty)
                      (x:xs) -> foldl (evalFold (HiExprValue (HiValueFunction f))) (Right x) xs
  | otherwise = Left HiErrorInvalidArgument
evalFFold [_, _]                             = Left HiErrorInvalidArgument
evalFFold _                                  = Left HiErrorArityMismatch

evalFold :: HiExpr -> Either HiError HiValue -> HiValue -> Either HiError HiValue
evalFold _ left@(Left _) _    = left
evalFold f (Right left) right = evalNoMonadFold f [left, right]

evalFPackBytes :: [HiValue] -> Either HiError HiValue
evalFPackBytes [HiValueList lst] =
  let bytes = getBytesFromList lst
    in case bytes of
      Left err -> Left err
      Right bs -> Right (HiValueBytes (Data.ByteString.pack bs))
evalFPackBytes [_] = Left HiErrorInvalidArgument
evalFPackBytes _ = Left HiErrorArityMismatch

evalFUnpackBytes :: [HiValue] -> Either HiError HiValue
evalFUnpackBytes [HiValueBytes bytes] = Right (HiValueList (Data.Sequence.fromList (map HiValueNumber (getIntListFromBytes (Data.ByteString.unpack bytes)))))
evalFUnpackBytes [_] = Left HiErrorInvalidArgument
evalFUnpackBytes _ = Left HiErrorArityMismatch

evalFEncodeUtf8 :: [HiValue] -> Either HiError HiValue
evalFEncodeUtf8 [HiValueString txt] = Right (HiValueBytes (Data.Text.Encoding.encodeUtf8 txt))
evalFEncodeUtf8 [_]                 = Left HiErrorInvalidArgument
evalFEncodeUtf8 _                   = Left HiErrorArityMismatch

evalFDecodeUtf8 :: [HiValue] -> Either HiError HiValue
evalFDecodeUtf8 [HiValueBytes txt] =
  let d = Data.Text.Encoding.decodeUtf8' txt
    in case d of
      Left _   -> Right HiValueNull
      Right dt -> Right (HiValueString dt)
evalFDecodeUtf8 [_] = Left HiErrorInvalidArgument
evalFDecodeUtf8 _ = Left HiErrorArityMismatch

evalFZip :: [HiValue] -> Either HiError HiValue
evalFZip [HiValueBytes bytes] = Right (HiValueBytes (toStrict (Codec.Compression.Zlib.compressWith zlibBestCompression (fromStrict bytes))))
evalFZip [_] = Left HiErrorInvalidArgument
evalFZip _ = Left HiErrorArityMismatch

evalFUnzip :: [HiValue] -> Either HiError HiValue
evalFUnzip [HiValueBytes bytes] = Right (HiValueBytes (toStrict (Codec.Compression.Zlib.decompress (fromStrict bytes))))
evalFUnzip [_] = Left HiErrorInvalidArgument
evalFUnzip _ = Left HiErrorArityMismatch

evalFSerialise :: [HiValue] -> Either HiError HiValue
evalFSerialise [v] = Right (HiValueBytes (toStrict (serialise v)))
evalFSerialise _   = Left HiErrorArityMismatch

evalFDeserialise :: [HiValue] -> Either HiError HiValue
evalFDeserialise [HiValueBytes bytes] = Right (deserialise (fromStrict bytes))
evalFDeserialise [_]                  = Left HiErrorInvalidArgument
evalFDeserialise _                    = Left HiErrorArityMismatch

evalFRead :: [HiValue] -> Either HiError HiValue
evalFRead [HiValueString pathTo] = Right (HiValueAction (HiActionRead (Data.Text.unpack pathTo)))
evalFRead [_]                    = Left HiErrorInvalidArgument
evalFRead _                      = Left HiErrorArityMismatch

evalFWrite :: [HiValue] -> Either HiError HiValue
evalFWrite [HiValueString pathTo, HiValueString str] = Right (HiValueAction (HiActionWrite (Data.Text.unpack pathTo) (Data.Text.encodeUtf8 str)))
evalFWrite [_] = Left HiErrorInvalidArgument
evalFWrite _ = Left HiErrorArityMismatch

evalFMkDir :: [HiValue] -> Either HiError HiValue
evalFMkDir [HiValueString pathTo] = Right (HiValueAction (HiActionMkDir (Data.Text.unpack pathTo)))
evalFMkDir [_]                    = Left HiErrorInvalidArgument
evalFMkDir _                      = Left HiErrorArityMismatch

evalFChDir :: [HiValue] -> Either HiError HiValue
evalFChDir [HiValueString pathTo] = Right (HiValueAction (HiActionChDir (Data.Text.unpack pathTo)))
evalFChDir [_]                    = Left HiErrorInvalidArgument
evalFChDir _                      = Left HiErrorArityMismatch

evalStrSlice :: Text -> [HiValue] -> Either HiError HiValue
evalStrSlice str [HiValueNumber left] =
  let mi = getIntegerFromRatio left
    in case mi of
      Nothing -> Left HiErrorInvalidArgument
      Just i -> let ri = fromIntegral i in
                  if i < 0 || ri >= Data.Text.length str
                  then Right HiValueNull
                  else Right (HiValueString (getSubstringFromText ri (ri + 1) str))
evalStrSlice str [HiValueNumber left, HiValueNumber right] =
  let (ml, mr) = (getIntegerFromRatio left, getIntegerFromRatio right)
    in case (ml, mr) of
      (Nothing, Nothing) -> Left HiErrorInvalidArgument
      (Nothing, _) -> Left HiErrorInvalidArgument
      (_, Nothing) -> Left HiErrorInvalidArgument
      (Just l, Just r) -> let (rl, rr) = (fromIntegral l, fromIntegral r) in
                            Right (HiValueString (getSubstringFromText rl rr str))
evalStrSlice str [left@(HiValueNumber _), HiValueNull] = evalStrSlice str [left, HiValueNumber (toRational (Data.Text.length str))]
evalStrSlice str [HiValueNull, right@(HiValueNumber _)] = evalStrSlice str [HiValueNumber 0, right]
evalStrSlice _ [_, _] = Left HiErrorInvalidArgument
evalStrSlice _ _ = Left HiErrorArityMismatch

evalListSlice :: Data.Sequence.Seq HiValue -> [HiValue] -> Either HiError HiValue
evalListSlice lst [HiValueNumber left] =
  let iLeft = getIntegerFromRatio left
    in case iLeft of
      Nothing -> Left HiErrorInvalidArgument
      Just i -> let e = Data.Sequence.lookup (fromIntegral i) lst
                  in case e of
                    Nothing -> Right HiValueNull
                    Just el -> Right el
evalListSlice lst [HiValueNumber left, HiValueNumber right] =
  let (iLeft, iRight) = (getIntegerFromRatio left, getIntegerFromRatio right)
    in case (iLeft, iRight) of
      (Nothing, Nothing) -> Left HiErrorInvalidArgument
      (Just _, Nothing) -> Left HiErrorInvalidArgument
      (Nothing, Just _) -> Left HiErrorInvalidArgument
      (Just il, Just ir) -> let (rl, rr) = (fromIntegral il, fromIntegral ir) in
                              Right (HiValueList (getSubsequenceFromSeq rl rr lst))
evalListSlice _ [_, _] = Left HiErrorInvalidArgument
evalListSlice _ _ = Left HiErrorArityMismatch

evalBytesSlice :: ByteString -> [HiValue] -> Either HiError HiValue
evalBytesSlice bs [HiValueNumber left] =
  let iLeft = getIntegerFromRatio left
    in case iLeft of
      Nothing -> Left HiErrorInvalidArgument
      Just i -> if Data.ByteString.length bs > fromIntegral i && i >= 0
                then Right (HiValueNumber (fromIntegral (Data.ByteString.index bs (fromIntegral i))))
                else Right HiValueNull
evalBytesSlice bs [HiValueNumber left, HiValueNumber right] =
  let (iLeft, iRight) = (getIntegerFromRatio left, getIntegerFromRatio right)
    in case (iLeft, iRight) of
      (Nothing, Nothing) -> Left HiErrorInvalidArgument
      (Just _, Nothing) -> Left HiErrorInvalidArgument
      (Nothing, Just _) -> Left HiErrorInvalidArgument
      (Just il, Just ir) -> let (rl, rr) = (fromIntegral il, fromIntegral ir) in
                              Right (HiValueBytes (getSubbytesFromByteString rl rr bs))
evalBytesSlice _ [_, _] = Left HiErrorInvalidArgument
evalBytesSlice _ _ = Left HiErrorArityMismatch

