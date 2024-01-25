{-# LANGUAGE DeriveGeneric #-}
module HW5.Base
  ( HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , isBinFun
  , HiMonad(..)
  , HiAction(..)
  ) where
import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data HiFun =
    HiFunAdd
  | HiFunSub
  | HiFunMul
  | HiFunDiv
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Generic)

data HiValue =
    HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic)

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)

-- | Determines if `HiFun` is binary, or not.
isBinFun :: HiFun -- ^ `HiFun` argument to determine
          -> Bool -- ^ `Bool` return value
isBinFun HiFunAdd            = True
isBinFun HiFunSub            = True
isBinFun HiFunMul            = True
isBinFun HiFunDiv            = True
isBinFun HiFunNot            = False
isBinFun HiFunAnd            = True
isBinFun HiFunOr             = True
isBinFun HiFunLessThan       = True
isBinFun HiFunGreaterThan    = True
isBinFun HiFunEquals         = True
isBinFun HiFunNotLessThan    = True
isBinFun HiFunNotGreaterThan = True
isBinFun HiFunNotEquals      = True
isBinFun HiFunIf             = False
isBinFun HiFunLength         = False
isBinFun HiFunToUpper        = False
isBinFun HiFunToLower        = False
isBinFun HiFunReverse        = False
isBinFun HiFunTrim           = False
isBinFun HiFunList           = False
isBinFun HiFunRange          = True
isBinFun HiFunFold           = True
isBinFun HiFunPackBytes      = False
isBinFun HiFunUnpackBytes    = False
isBinFun HiFunEncodeUtf8     = False
isBinFun HiFunDecodeUtf8     = False
isBinFun HiFunZip            = False
isBinFun HiFunUnzip          = False
isBinFun HiFunSerialise      = False
isBinFun HiFunDeserialise    = False
isBinFun HiFunRead           = False
isBinFun HiFunWrite          = False
isBinFun HiFunMkDir          = False
isBinFun HiFunChDir          = False
isBinFun HiFunParseTime      = False
isBinFun HiFunRand           = True
isBinFun HiFunEcho           = False
isBinFun HiFunCount          = False
isBinFun HiFunKeys           = False
isBinFun HiFunValues         = False
isBinFun HiFunInvert         = False

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

instance Serialise HiFun
instance Serialise HiAction
instance Serialise HiValue

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
