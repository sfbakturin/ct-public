module HW5.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (compressWith, decompress)
import Codec.Serialise (deserialise, serialise)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Either (isLeft, rights)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map
import Data.Semigroup
import Data.Sequence (Seq, empty, fromList, length, lookup, reverse, (><))
import qualified Data.Set
import Data.Text (Text, concat, count, length, pack, reverse, strip, toLower, toUpper, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Text.Encoding as Data.Text
import Data.Time (UTCTime (..), addUTCTime, diffUTCTime)
import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..),
                 isBinFun)
import HW5.EvaluatorCommon (countFromArray, evalRangeFromTo, firstError, getBytesFromList,
                            getIntListFromBytes, getIntegerFromRatio, getNormalFromDiff,
                            getSubbytesFromByteString, getSubsequenceFromSeq, getSubstringFromText,
                            maybeRatToInt, selectAssocDict, splitAllChars, zlibBestCompression)
import HW5.EvaluatorNonMonad (evalNoMonadFold)
import Text.Read (readMaybe)

-- The line below is commented to prevent build error
-- in project template because of 'redundant constraint' warning.
-- Uncomment this line and use correct function signature
-- while doing the homework.

-- | Evaluate input expression.
-- If expression is correct, then returns evaluated `HiValue` value
-- Otherwise, return error as `HiError`
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprDict dict) = do
  let leftsRaw = map fst dict
      rightsRaw = map snd dict
  evaluatedLefts <- traverse eval leftsRaw
  evaluatedRights <- traverse eval rightsRaw
  if any isLeft evaluatedLefts
  then return (Left (firstError evaluatedLefts))
  else if any isLeft evaluatedRights
        then return (Left (firstError evaluatedRights))
        else return (Right (HiValueDict (Data.Map.fromList (zip (rights evaluatedLefts) (rights evaluatedRights)))))
eval (HiExprValue val) = pure (Right val)
eval (HiExprRun runMe) = do
  evaluated <- eval runMe
  case evaluated of
    err@(Left _)              -> pure err
    Right (HiValueAction act) -> Right <$> runAction act
    Right _                   -> pure (Left HiErrorInvalidArgument)
eval (HiExprApply fun@(HiExprValue (HiValueFunction HiFunIf)) args) = evalLazy fun args
eval (HiExprApply fun@(HiExprValue (HiValueFunction HiFunAnd)) args) = evalLazy fun args
eval (HiExprApply fun@(HiExprValue (HiValueFunction HiFunOr)) args) = evalLazy fun args
eval (HiExprApply fun args) = do
  evaluated <- traverse eval args
  if any isLeft evaluated
  then return (Left (firstError evaluated))
  else evalChoose fun (rights evaluated)

evalLazy :: HiMonad m => HiExpr -> [HiExpr] -> m (Either HiError HiValue)
evalLazy (HiExprValue (HiValueFunction HiFunIf)) args  = evalFIf args
evalLazy (HiExprValue (HiValueFunction HiFunAnd)) args = evalFAnd args
evalLazy (HiExprValue (HiValueFunction HiFunOr)) args  = evalFOr args
evalLazy _ _                                           = pure (Left HiErrorInvalidArgument)

evalChoose :: HiMonad m => HiExpr -> [HiValue] -> m (Either HiError HiValue)
evalChoose left@(HiExprApply _ _) args = do
  evaluated <- eval left
  case evaluated of
    err@(Left _)    -> pure err
    (Right success) -> evalChoose (HiExprValue success) args
evalChoose left@(HiExprDict _) args = do
  evaluated <- eval left
  case evaluated of
    err@(Left _)    -> pure err
    (Right success) -> evalChoose (HiExprValue success) args
evalChoose (HiExprValue (HiValueFunction HiFunAdd)) args = evalFAdd args
evalChoose (HiExprValue (HiValueFunction HiFunSub)) args = evalFSub args
evalChoose (HiExprValue (HiValueFunction HiFunMul)) args = evalFMul args
evalChoose (HiExprValue (HiValueFunction HiFunDiv)) args = evalFDiv args
evalChoose (HiExprValue (HiValueFunction HiFunNot)) args = evalFNot args
evalChoose (HiExprValue (HiValueFunction HiFunLessThan)) args = evalFLessThan args
evalChoose (HiExprValue (HiValueFunction HiFunGreaterThan)) args = evalFGreaterThan args
evalChoose (HiExprValue (HiValueFunction HiFunEquals)) args = evalFEquals args
evalChoose (HiExprValue (HiValueFunction HiFunNotLessThan)) args = evalFNotLessThan args
evalChoose (HiExprValue (HiValueFunction HiFunNotGreaterThan)) args = evalFNotGreaterThan args
evalChoose (HiExprValue (HiValueFunction HiFunNotEquals)) args = evalFNotEquals args
evalChoose (HiExprValue (HiValueFunction HiFunLength)) args = evalFLength args
evalChoose (HiExprValue (HiValueFunction HiFunToUpper)) args = evalFToUpper args
evalChoose (HiExprValue (HiValueFunction HiFunToLower)) args = evalFToLower args
evalChoose (HiExprValue (HiValueFunction HiFunReverse)) args = evalFReverse args
evalChoose (HiExprValue (HiValueFunction HiFunTrim)) args = evalFTrim args
evalChoose (HiExprValue (HiValueFunction HiFunList)) args = evalFList args
evalChoose (HiExprValue (HiValueFunction HiFunRange)) args = evalFRange args
evalChoose (HiExprValue (HiValueFunction HiFunFold)) args = evalFFold args
evalChoose (HiExprValue (HiValueFunction HiFunPackBytes)) args = evalFPackBytes args
evalChoose (HiExprValue (HiValueFunction HiFunUnpackBytes)) args = evalFUnpackBytes args
evalChoose (HiExprValue (HiValueFunction HiFunEncodeUtf8)) args = evalFEncodeUtf8 args
evalChoose (HiExprValue (HiValueFunction HiFunDecodeUtf8)) args = evalFDecodeUtf8 args
evalChoose (HiExprValue (HiValueFunction HiFunZip)) args = evalFZip args
evalChoose (HiExprValue (HiValueFunction HiFunUnzip)) args = evalFUnzip args
evalChoose (HiExprValue (HiValueFunction HiFunSerialise)) args = evalFSerialise args
evalChoose (HiExprValue (HiValueFunction HiFunDeserialise)) args = evalFDeserialise args
evalChoose (HiExprValue (HiValueFunction HiFunRead)) args =  evalFRead args
evalChoose (HiExprValue (HiValueFunction HiFunWrite)) args = evalFWrite args
evalChoose (HiExprValue (HiValueFunction HiFunMkDir)) args = evalFMkDir args
evalChoose (HiExprValue (HiValueFunction HiFunChDir)) args = evalFChDir args
evalChoose (HiExprValue (HiValueFunction HiFunParseTime)) args = evalFParseTime args
evalChoose (HiExprValue (HiValueFunction HiFunRand)) args = evalFRand args
evalChoose (HiExprValue (HiValueFunction HiFunEcho)) args = evalFEcho args
evalChoose (HiExprValue (HiValueFunction HiFunCount)) args = evalFCount args
evalChoose (HiExprValue (HiValueFunction HiFunKeys)) args = evalFKeys args
evalChoose (HiExprValue (HiValueFunction HiFunValues)) args = evalFValues args
evalChoose (HiExprValue (HiValueFunction HiFunInvert)) args = evalFInvert args
evalChoose (HiExprValue (HiValueString s)) args = evalStrSlice s args
evalChoose (HiExprValue (HiValueList lst)) args = evalListSlice lst args
evalChoose (HiExprValue (HiValueBytes bs)) args = evalBytesSlice bs args
evalChoose (HiExprValue (HiValueDict dict)) args = evalDictSlice dict args
evalChoose _ _ = pure (Left HiErrorInvalidFunction)

evalFAdd :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFAdd [HiValueNumber left, HiValueNumber right] = pure (Right (HiValueNumber (left + right)))
evalFAdd [HiValueString left, HiValueString right] = pure (Right (HiValueString (Data.Text.concat [left, right])))
evalFAdd [HiValueList left, HiValueList right] = pure (Right (HiValueList (left Data.Sequence.>< right)))
evalFAdd [HiValueBytes left, HiValueBytes right] = pure (Right (HiValueBytes (left <> right)))
evalFAdd [HiValueTime left, HiValueNumber right] = pure (Right (HiValueTime (addUTCTime (fromRational right) left)))
evalFAdd [_, _] = pure(Left HiErrorInvalidArgument)
evalFAdd _ = pure (Left HiErrorArityMismatch)

evalFSub :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFSub [HiValueNumber left, HiValueNumber right] = pure (Right (HiValueNumber (left - right)))
evalFSub [HiValueTime left, HiValueNumber right] = pure (Right (HiValueNumber (toRational (diffUTCTime left (getNormalFromDiff (fromRational right))))))
evalFSub [HiValueTime left, HiValueTime right] = pure (Right (HiValueNumber (toRational (diffUTCTime left right))))
evalFSub [_, _]                                    = pure (Left HiErrorInvalidArgument)
evalFSub _                                         = pure (Left HiErrorArityMismatch)

evalFMul :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFMul [HiValueNumber left, HiValueNumber right] = pure (Right (HiValueNumber (left * right)))
evalFMul [HiValueString left, HiValueNumber right] =
  let i = getIntegerFromRatio right
    in case i of
      Nothing -> pure (Left HiErrorInvalidArgument)
      Just c  -> pure (Right (HiValueString (pack (stimes c (unpack left)))))
evalFMul [HiValueList left, HiValueNumber right] =
  let i = getIntegerFromRatio right
    in case i of
      Nothing -> pure (Left HiErrorInvalidArgument)
      Just c  -> pure (Right (HiValueList (stimes c left)))
evalFMul [HiValueBytes left, HiValueNumber right] =
  let i = getIntegerFromRatio right
    in case i of
      Nothing -> pure (Left HiErrorInvalidArgument)
      Just c  -> pure (Right (HiValueBytes (stimes c left)))
evalFMul [_, _] = pure (Left HiErrorInvalidArgument)
evalFMul _ = pure (Left HiErrorArityMismatch)

evalFDiv :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFDiv [HiValueNumber left, HiValueNumber right]
  | right == 0 = pure (Left HiErrorDivideByZero)
  | otherwise = pure (Right (HiValueNumber (left / right)))
evalFDiv [HiValueString left, HiValueString right] = pure (Right (HiValueString (Data.Text.concat [left, pack "/", right])))
evalFDiv [_, _] = pure (Left HiErrorInvalidArgument)
evalFDiv _ = pure (Left HiErrorArityMismatch)

evalFNot :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFNot [HiValueBool left] = pure (Right (HiValueBool (not left)))
evalFNot [_]                = pure (Left HiErrorInvalidArgument)
evalFNot _                  = pure (Left HiErrorArityMismatch)

evalFAnd :: HiMonad m => [HiExpr] -> m (Either HiError HiValue)
evalFAnd [left, right] = do
  evaluatedLeft <- eval left
  case evaluatedLeft of
    err@(Left _)                    -> pure err
    ret@(Right HiValueNull)         -> pure ret
    ret@(Right (HiValueBool False)) -> pure ret
    (Right _)                       -> eval right
evalFAnd _ = pure (Left HiErrorArityMismatch)

evalFOr :: HiMonad m => [HiExpr] -> m (Either HiError HiValue)
evalFOr [left, right] = do
  evaluatedLeft <- eval left
  case evaluatedLeft of
    err@(Left _)                -> pure err
    (Right HiValueNull)         -> eval right
    (Right (HiValueBool False)) -> eval right
    ret@(Right _)               -> pure ret
evalFOr _ = pure (Left HiErrorArityMismatch)

evalFLessThan :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFLessThan [HiValueNumber left, HiValueNumber right] = pure (Right (HiValueBool (left < right)))
evalFLessThan [HiValueNumber _, HiValueBool _]          = pure (Right (HiValueBool False))
evalFLessThan [HiValueBool _, HiValueNumber _]          = pure (Right (HiValueBool True))
evalFLessThan [HiValueBool left, HiValueBool right]     = pure (Right (HiValueBool (left < right)))
evalFLessThan [_, _]                                    = pure (Left HiErrorInvalidArgument)
evalFLessThan _                                         = pure (Left HiErrorArityMismatch)

evalFGreaterThan :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFGreaterThan [HiValueNumber left, HiValueNumber right] = pure (Right (HiValueBool (left > right)))
evalFGreaterThan [HiValueNumber _, HiValueBool _]          = pure (Right (HiValueBool True))
evalFGreaterThan [HiValueBool _, HiValueNumber _]          = pure (Right (HiValueBool False))
evalFGreaterThan [HiValueBool left, HiValueBool right]     = pure (Right (HiValueBool (left > right)))
evalFGreaterThan [_, _]                                    = pure (Left HiErrorInvalidArgument)
evalFGreaterThan _                                         = pure (Left HiErrorArityMismatch)

evalFEquals :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFEquals [HiValueNumber left, HiValueNumber right]     = pure (Right (HiValueBool (left == right)))
evalFEquals [HiValueString left, HiValueString right]     = pure (Right (HiValueBool (left == right)))
evalFEquals [HiValueNumber _, HiValueBool _]              = pure (Right (HiValueBool False))
evalFEquals [HiValueBool _, HiValueNumber _]              = pure (Right (HiValueBool False))
evalFEquals [HiValueBool left, HiValueBool right]         = pure (Right (HiValueBool (left == right)))
evalFEquals [HiValueFunction left, HiValueFunction right] = pure (Right (HiValueBool (left == right)))
evalFEquals [HiValueList left, HiValueList right]         = pure (Right (HiValueBool (left == right)))
evalFEquals [_, _]                                        = pure (Left HiErrorInvalidArgument)
evalFEquals _                                             = pure (Left HiErrorArityMismatch)

evalFNotLessThan :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFNotLessThan [HiValueNumber left, HiValueNumber right] = pure (Right (HiValueBool (left >= right)))
evalFNotLessThan [HiValueNumber _, HiValueBool _]          = pure (Right (HiValueBool True))
evalFNotLessThan [HiValueBool _, HiValueNumber _]          = pure (Right (HiValueBool False))
evalFNotLessThan [HiValueBool left, HiValueBool right]     = pure (Right (HiValueBool (left >= right)))
evalFNotLessThan [_, _]                                    = pure (Left HiErrorInvalidArgument)
evalFNotLessThan _                                         = pure (Left HiErrorArityMismatch)

evalFNotGreaterThan :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFNotGreaterThan [HiValueNumber left, HiValueNumber right] = pure (Right (HiValueBool (left <= right)))
evalFNotGreaterThan [HiValueNumber _, HiValueBool _]          = pure (Right (HiValueBool False))
evalFNotGreaterThan [HiValueBool _, HiValueNumber _]          = pure (Right (HiValueBool True))
evalFNotGreaterThan [HiValueBool left, HiValueBool right]     = pure (Right (HiValueBool (left <= right)))
evalFNotGreaterThan [_, _]                                    = pure (Left HiErrorInvalidArgument)
evalFNotGreaterThan _                                         = pure (Left HiErrorArityMismatch)

evalFNotEquals :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFNotEquals [HiValueNumber left, HiValueNumber right]     = pure (Right (HiValueBool (left /= right)))
evalFNotEquals [HiValueString left, HiValueString right]     = pure (Right (HiValueBool (left /= right)))
evalFNotEquals [HiValueNumber _, HiValueBool _]              = pure (Right (HiValueBool True))
evalFNotEquals [HiValueBool _, HiValueNumber _]              = pure (Right (HiValueBool True))
evalFNotEquals [HiValueBool left, HiValueBool right]         = pure (Right (HiValueBool (left /= right)))
evalFNotEquals [HiValueFunction left, HiValueFunction right] = pure (Right (HiValueBool (left /= right)))
evalFNotEquals [HiValueList left, HiValueList right]         = pure (Right (HiValueBool (left /= right)))
evalFNotEquals [_, _]                                        = pure (Left HiErrorInvalidArgument)
evalFNotEquals _                                             = pure (Left HiErrorArityMismatch)

evalFIf :: HiMonad m => [HiExpr] -> m (Either HiError HiValue)
evalFIf [predicate, left, right] = do
  evaluatedPredicate <- eval predicate
  case evaluatedPredicate of
    err@(Left _)              -> pure err
    Right (HiValueBool True)  -> eval left
    Right (HiValueBool False) -> eval right
    Right _                   -> pure (Left HiErrorInvalidArgument)
evalFIf _ = pure (Left HiErrorArityMismatch)

evalFLength :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFLength [HiValueString s] = pure (Right (HiValueNumber (toRational (Data.Text.length s))))
evalFLength [HiValueList lst] = pure (Right (HiValueNumber (toRational (Data.Sequence.length lst))))
evalFLength [_]               = pure (Left HiErrorInvalidArgument)
evalFLength _                 = pure (Left HiErrorArityMismatch)

evalFToUpper :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFToUpper [HiValueString s] = pure (Right (HiValueString (toUpper s)))
evalFToUpper [_]               = pure (Left HiErrorInvalidArgument)
evalFToUpper _                 = pure (Left HiErrorArityMismatch)

evalFToLower :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFToLower [HiValueString s] = pure (Right (HiValueString (toLower s)))
evalFToLower [_]               = pure (Left HiErrorInvalidArgument)
evalFToLower _                 = pure (Left HiErrorArityMismatch)

evalFReverse :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFReverse [HiValueString s] = pure (Right (HiValueString (Data.Text.reverse s)))
evalFReverse [HiValueList lst] = pure (Right (HiValueList (Data.Sequence.reverse lst)))
evalFReverse [_]               = pure (Left HiErrorInvalidArgument)
evalFReverse _                 = pure (Left HiErrorArityMismatch)

evalFTrim :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFTrim [HiValueString s] = pure (Right (HiValueString (strip s)))
evalFTrim [_]               = pure (Left HiErrorInvalidArgument)
evalFTrim _                 = pure (Left HiErrorArityMismatch)

evalFList :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFList elements = pure (Right (HiValueList (Data.Sequence.fromList elements)))

evalFRange :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFRange [HiValueNumber from, HiValueNumber to] = pure (Right (HiValueList (Data.Sequence.fromList (evalRangeFromTo from to))))
evalFRange [_, _] = pure (Left HiErrorInvalidArgument)
evalFRange _ = pure (Left HiErrorArityMismatch)

evalFFold :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFFold [HiValueFunction f, HiValueList lst]
  | isBinFun f = let args = toList lst
                    in case args of
                      []     -> pure (Right (HiValueList Data.Sequence.empty))
                      (x:xs) -> pure (foldl (evalFold (HiExprValue (HiValueFunction f))) (Right x) xs)
  | otherwise = pure (Left HiErrorInvalidArgument)
evalFFold [_, _]                             = pure (Left HiErrorInvalidArgument)
evalFFold _                                  = pure (Left HiErrorArityMismatch)

evalFold :: HiExpr -> Either HiError HiValue -> HiValue -> Either HiError HiValue
evalFold _ left@(Left _) _    = left
evalFold f (Right left) right = evalNoMonadFold f [left, right]

evalFPackBytes :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFPackBytes [HiValueList lst] =
  let bytes = getBytesFromList lst
    in case bytes of
      Left err -> pure (Left err)
      Right bs -> pure (Right (HiValueBytes (Data.ByteString.pack bs)))
evalFPackBytes [_] = pure (Left HiErrorInvalidArgument)
evalFPackBytes _ = pure (Left HiErrorArityMismatch)

evalFUnpackBytes :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFUnpackBytes [HiValueBytes bytes] = pure (Right (HiValueList (Data.Sequence.fromList (map HiValueNumber (getIntListFromBytes (Data.ByteString.unpack bytes))))))
evalFUnpackBytes [_] = pure (Left HiErrorInvalidArgument)
evalFUnpackBytes _ = pure (Left HiErrorArityMismatch)

evalFEncodeUtf8 :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFEncodeUtf8 [HiValueString txt] = pure (Right (HiValueBytes (Data.Text.Encoding.encodeUtf8 txt)))
evalFEncodeUtf8 [_]                 = pure (Left HiErrorInvalidArgument)
evalFEncodeUtf8 _                   = pure (Left HiErrorArityMismatch)

evalFDecodeUtf8 :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFDecodeUtf8 [HiValueBytes txt] =
  let d = Data.Text.Encoding.decodeUtf8' txt
    in case d of
      Left _   -> pure (Right HiValueNull)
      Right dt -> pure (Right (HiValueString dt))
evalFDecodeUtf8 [_] = pure (Left HiErrorInvalidArgument)
evalFDecodeUtf8 _ = pure (Left HiErrorArityMismatch)

evalFZip :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFZip [HiValueBytes bytes] = pure (Right (HiValueBytes (toStrict (Codec.Compression.Zlib.compressWith zlibBestCompression (fromStrict bytes)))))
evalFZip [_] = pure (Left HiErrorInvalidArgument)
evalFZip _ = pure (Left HiErrorArityMismatch)

evalFUnzip :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFUnzip [HiValueBytes bytes] = pure (Right (HiValueBytes (toStrict (Codec.Compression.Zlib.decompress (fromStrict bytes)))))
evalFUnzip [_] = pure (Left HiErrorInvalidArgument)
evalFUnzip _ = pure (Left HiErrorArityMismatch)

evalFSerialise :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFSerialise [v] = pure (Right (HiValueBytes (toStrict (serialise v))))
evalFSerialise _   = pure (Left HiErrorArityMismatch)

evalFDeserialise :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFDeserialise [HiValueBytes bytes] = pure (Right (deserialise (fromStrict bytes)))
evalFDeserialise [_]                  = pure (Left HiErrorInvalidArgument)
evalFDeserialise _                    = pure (Left HiErrorArityMismatch)

evalFRead :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFRead [HiValueString pathTo] = pure (Right (HiValueAction (HiActionRead (Data.Text.unpack pathTo))))
evalFRead [_]                    = pure (Left HiErrorInvalidArgument)
evalFRead _                      = pure (Left HiErrorArityMismatch)

evalFWrite :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFWrite [HiValueString pathTo, HiValueString str] = pure (Right (HiValueAction (HiActionWrite (Data.Text.unpack pathTo) (Data.Text.encodeUtf8 str))))
evalFWrite [_] = pure (Left HiErrorInvalidArgument)
evalFWrite _ = pure (Left HiErrorArityMismatch)

evalFMkDir :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFMkDir [HiValueString pathTo] = pure (Right (HiValueAction (HiActionMkDir (Data.Text.unpack pathTo))))
evalFMkDir [_]                    = pure (Left HiErrorInvalidArgument)
evalFMkDir _                      = pure (Left HiErrorArityMismatch)

evalFChDir :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFChDir [HiValueString pathTo] = pure (Right (HiValueAction (HiActionChDir (Data.Text.unpack pathTo))))
evalFChDir [_]                    = pure (Left HiErrorInvalidArgument)
evalFChDir _                      = pure (Left HiErrorArityMismatch)

evalFParseTime :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFParseTime [HiValueString s] = do
  let maybeTime = (readMaybe (Data.Text.unpack s) :: Maybe UTCTime)
    in case maybeTime of
      Nothing         -> pure (Right HiValueNull)
      Just parsedTime -> pure (Right (HiValueTime parsedTime))
evalFParseTime [_]                    = pure (Left HiErrorInvalidArgument)
evalFParseTime _                      = pure (Left HiErrorArityMismatch)

evalFRand :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFRand [HiValueNumber start, HiValueNumber end] = do
  let startInt = maybeRatToInt start
      endInt   = maybeRatToInt end
  case (startInt, endInt) of
    (Just s, Just e) -> pure (Right (HiValueAction (HiActionRand s e)))
    (_, _)           -> pure (Left HiErrorInvalidArgument)
evalFRand [_, _] = pure (Left HiErrorInvalidArgument)
evalFRand _ = pure (Left HiErrorArityMismatch)

evalFEcho :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFEcho [HiValueString txt] = pure (Right (HiValueAction (HiActionEcho txt)))
evalFEcho [_]                 = pure (Left HiErrorInvalidArgument)
evalFEcho _                   = pure (Left HiErrorArityMismatch)

evalFCount :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFCount [HiValueString txt] = pure (Right (HiValueDict (Data.Map.fromList (map (\x -> ((HiValueString x), (HiValueNumber (fromIntegral (Data.Text.count x txt))))) (splitAllChars txt)))))
evalFCount [HiValueBytes bs] = pure (Right (HiValueDict (evalFCountImpl (map HiValueNumber (getIntListFromBytes (Data.ByteString.unpack bs))))))
evalFCount [HiValueList lst] = pure (Right (HiValueDict (evalFCountImpl (toList lst))))
evalFCount [_] = pure (Left HiErrorInvalidArgument)
evalFCount _ = pure (Left HiErrorArityMismatch)

evalFCountImpl :: [HiValue] -> Map HiValue HiValue
evalFCountImpl lst = foldl (\m e -> Data.Map.insert e (HiValueNumber (toRational (countFromArray e lst))) m) Data.Map.empty (Data.Set.fromList (toList lst))

evalFKeys :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFKeys [HiValueDict dict] = pure (Right (HiValueList (Data.Sequence.fromList (Data.Map.keys dict))))
evalFKeys [_] = pure (Left HiErrorInvalidArgument)
evalFKeys _ = pure (Left HiErrorArityMismatch)

evalFValues :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFValues [HiValueDict dict] = pure (Right (HiValueList (Data.Sequence.fromList (Data.Map.elems dict))))
evalFValues [_] = pure (Left HiErrorInvalidArgument)
evalFValues _ = pure (Left HiErrorArityMismatch)

evalFInvert :: HiMonad m => [HiValue] -> m (Either HiError HiValue)
evalFInvert [HiValueDict dict] = do
  let xs = map (\(x, y) -> (y, x)) (Data.Map.assocs dict)
      newKeys = Data.Set.fromList (map fst xs)
      ascMap = map (\x -> (fst x, HiValueList (fromList (snd x)))) (foldl (\acc k -> acc ++ [(k, selectAssocDict k xs)]) [] newKeys)
  pure (Right (HiValueDict (Data.Map.fromList ascMap)))
evalFInvert [_] = pure (Left HiErrorInvalidArgument)
evalFInvert _ = pure (Left HiErrorArityMismatch)

evalStrSlice :: HiMonad m => Text -> [HiValue] -> m (Either HiError HiValue)
evalStrSlice str [HiValueNumber left] =
  let mi = getIntegerFromRatio left
    in case mi of
      Nothing -> pure (Left HiErrorInvalidArgument)
      Just i -> let ri = fromIntegral i in
                  if i < 0 || ri >= Data.Text.length str
                  then pure (Right HiValueNull)
                  else pure (Right (HiValueString (getSubstringFromText ri (ri + 1) str)))
evalStrSlice str [HiValueNumber left, HiValueNumber right] =
  let (ml, mr) = (getIntegerFromRatio left, getIntegerFromRatio right)
    in case (ml, mr) of
      (Nothing, Nothing) -> pure (Left HiErrorInvalidArgument)
      (Nothing, _) -> pure (Left HiErrorInvalidArgument)
      (_, Nothing) -> pure (Left HiErrorInvalidArgument)
      (Just l, Just r) -> let (rl, rr) = (fromIntegral l, fromIntegral r) in
                            pure (Right (HiValueString (getSubstringFromText rl rr str)))
evalStrSlice str [left@(HiValueNumber _), HiValueNull] = evalStrSlice str [left, HiValueNumber (toRational (Data.Text.length str))]
evalStrSlice str [HiValueNull, right@(HiValueNumber _)] = evalStrSlice str [HiValueNumber 0, right]
evalStrSlice _ [_, _] = pure (Left HiErrorInvalidArgument)
evalStrSlice _ _ = pure (Left HiErrorArityMismatch)

evalListSlice :: HiMonad m => Data.Sequence.Seq HiValue -> [HiValue] -> m (Either HiError HiValue)
evalListSlice lst [HiValueNumber left] =
  let iLeft = getIntegerFromRatio left
    in case iLeft of
      Nothing -> pure (Left HiErrorInvalidArgument)
      Just i -> let e = Data.Sequence.lookup (fromIntegral i) lst
                  in case e of
                    Nothing -> pure (Right HiValueNull)
                    Just el -> pure (Right el)
evalListSlice lst [HiValueNumber left, HiValueNumber right] =
  let (iLeft, iRight) = (getIntegerFromRatio left, getIntegerFromRatio right)
    in case (iLeft, iRight) of
      (Nothing, Nothing) -> pure ( Left HiErrorInvalidArgument)
      (Just _, Nothing) ->  pure (Left HiErrorInvalidArgument)
      (Nothing, Just _) ->  pure (Left HiErrorInvalidArgument)
      (Just il, Just ir) -> let (rl, rr) = (fromIntegral il, fromIntegral ir) in
                              pure (Right (HiValueList (getSubsequenceFromSeq rl rr lst)))
evalListSlice _ [_, _] = pure (Left HiErrorInvalidArgument)
evalListSlice _ _ = pure (Left HiErrorArityMismatch)

evalBytesSlice :: HiMonad m => ByteString -> [HiValue] -> m (Either HiError HiValue)
evalBytesSlice bs [HiValueNumber left] =
  let iLeft = getIntegerFromRatio left
    in case iLeft of
      Nothing -> pure (Left HiErrorInvalidArgument)
      Just i -> if Data.ByteString.length bs > fromIntegral i && i >= 0
                then pure (Right (HiValueNumber (fromIntegral (Data.ByteString.index bs (fromIntegral i)))))
                else pure (Right HiValueNull)
evalBytesSlice bs [HiValueNumber left, HiValueNumber right] =
  let (iLeft, iRight) = (getIntegerFromRatio left, getIntegerFromRatio right)
    in case (iLeft, iRight) of
      (Nothing, Nothing) -> pure ( Left HiErrorInvalidArgument)
      (Just _, Nothing) ->  pure (Left HiErrorInvalidArgument)
      (Nothing, Just _) ->  pure (Left HiErrorInvalidArgument)
      (Just il, Just ir) -> let (rl, rr) = (fromIntegral il, fromIntegral ir) in
                              pure (Right (HiValueBytes (getSubbytesFromByteString rl rr bs)))
evalBytesSlice _ [_, _] = pure (Left HiErrorInvalidArgument)
evalBytesSlice _ _ = pure (Left HiErrorArityMismatch)

evalDictSlice :: HiMonad m => Map HiValue HiValue -> [HiValue] -> m (Either HiError HiValue)
evalDictSlice dict [key] = case (Data.Map.lookup key dict) of
                            Nothing  -> pure (Left HiErrorInvalidArgument)
                            Just val -> pure (Right val)
evalDictSlice _ _ = pure (Left HiErrorArityMismatch)
