module HW5.Pretty
  ( prettyValue
  ) where

import Prettyprinter (Doc, Pretty (pretty))
import Prettyprinter.Render.Terminal (AnsiStyle)

import Data.Scientific (floatingOrInteger, fromRationalRepetendUnlimited)

import Data.Ratio (denominator, numerator)

import HW5.Base (HiAction (..), HiFun (..), HiValue (..))

import Data.ByteString (ByteString)
import qualified Data.ByteString
import qualified Data.Map
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Text (unpack)
import Text.Printf (printf)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- | Make `String` output of `HiValue` value.
-- If its `HiValueNumber`, then try to output as non-ratio or single integer, if fails, output as `prettyDQR`
-- If its `HiValueBool`, then output its `Bool` value as `String`
-- If its `HiValueNull`, then output "null"
-- If its `HiValueString`, then output its `Text` value as `String`
-- If its `HiValueFunction`, then output as `showFun`
-- If its `HiValueList`, then output empty list or as `showHiList`
-- If its `HiValueBytes`, then output as `showBytes`
-- If its `HiValueAction`, then output as `showAction`
-- If its `HiValueTime`, then output as REPL input of UTCTime
-- If its `HiValueDict`, then output in format '{' (<KEY> : <VALUE> (',' <KEY> : <VALUE>)*)? '}'
prettyValueString :: HiValue -- ^ `HiValue` argument to print output
                  -> String  -- ^ `String` return value
prettyValueString (HiValueNumber num) =
  let (n, idx) = fromRationalRepetendUnlimited num
    in case idx of
      Nothing -> case (floatingOrInteger n :: Either Double Integer) of
                  Left f  -> show f
                  Right i -> show i
      Just _ -> let (nm, dn) = quotRem (numerator num) (denominator num)
        in prettyDQR (denominator num) nm dn
prettyValueString (HiValueBool True) = "true"
prettyValueString (HiValueBool False) = "false"
prettyValueString HiValueNull = "null"
prettyValueString (HiValueString txt) = "\"" ++ unpack txt ++ "\""
prettyValueString (HiValueFunction fun) = showFun fun
prettyValueString (HiValueList lst) = case lst of
                                        lstHead :<| lstTail -> "[" ++ " " ++ showHiList lstHead lstTail
                                        Empty -> "[" ++ " " ++ "]"
prettyValueString (HiValueBytes bs) = showBytes bs
prettyValueString (HiValueAction act) = showAction act
prettyValueString (HiValueTime time) = "parse-time" ++ "(" ++ "\"" ++ show time ++ "\"" ++ ")"
prettyValueString (HiValueDict dict) = case Data.Map.assocs dict of
                                        (x:xs) -> "{" ++ showDict x xs ++ " " ++ "}"
                                        []     -> "{" ++ " " ++ "}"

-- | Make `String` output fraction as sum of proper fraction and integer part of a number.
-- If rem is 0, then output as single integer
-- If quot is 0, then output rem and denominator as proper fraction
-- Otherwise, output as sum of proper fraction and single integer
prettyDQR :: Integer -- ^ `Integer` argument denominator of fraction
          -> Integer -- ^ `Integer` argument quot of fraction
          -> Integer -- ^ `Integer` argument rem of fraction
          -> String  -- ^ `String` return value
prettyDQR _ q 0 = show q
prettyDQR d 0 r
  | r > 0 = show r ++ "/" ++ show d
  | otherwise = "-" ++ show (abs r) ++ "/" ++ show d
prettyDQR d q r
  | r < 0 = show q ++ " - " ++ show (abs r) ++ "/" ++ show d
  | otherwise = show q ++ " + " ++ show r ++ "/" ++ show d

-- | Show `HiValue` value.
prettyValue :: HiValue       -- ^ `HiValue` argument to print output
            -> Doc AnsiStyle -- ^ `Doc AnsiStyle` return value
prettyValue v = pretty (prettyValueString v)

-- | Make `String` output of list value.
-- If rest part is `Empty`, then output only first argument
-- Otherwise, output current head and rest part
showHiList :: HiValue                    -- ^ `HiValue` argument head of list.
            -> Data.Sequence.Seq HiValue -- ^ `Seq HiValue` argument rest part of list.
            -> String                    -- ^ `String` return value
showHiList lstHead lstTail = case lstTail of
                              lstTailHead :<| lstTailTail -> prettyValueString lstHead ++ "," ++ " " ++ showHiList lstTailHead lstTailTail
                              Empty -> prettyValueString lstHead ++ " " ++ "]"

-- | Make `String` output of function value.
showFun :: HiFun  -- ^ `HiFun` argument to output
        -> String -- ^ `String` return value
showFun HiFunAdd            =  "add"
showFun HiFunSub            =  "sub"
showFun HiFunDiv            =  "div"
showFun HiFunMul            =  "mul"
showFun HiFunNot            =  "not"
showFun HiFunAnd            =  "and"
showFun HiFunOr             =  "or"
showFun HiFunLessThan       =  "less-than"
showFun HiFunGreaterThan    =  "greater-than"
showFun HiFunEquals         =  "equals"
showFun HiFunNotLessThan    =  "not-less-than"
showFun HiFunNotGreaterThan =  "not-greater-than"
showFun HiFunNotEquals      =  "not-equals"
showFun HiFunIf             =  "if"
showFun HiFunLength         =  "length"
showFun HiFunToUpper        =  "to-upper"
showFun HiFunToLower        =  "to-lower"
showFun HiFunReverse        =  "reverse"
showFun HiFunTrim           =  "trim"
showFun HiFunList           =  "li objectst"
showFun HiFunRange          =  "range"
showFun HiFunFold           =  "fold"
showFun HiFunPackBytes      =  "pack-bytes"
showFun HiFunUnpackBytes    =  "unpack-bytes"
showFun HiFunEncodeUtf8     =  "encode-utf8"
showFun HiFunDecodeUtf8     =  "decode-utf8"
showFun HiFunZip            =  "zip"
showFun HiFunUnzip          =  "unzip"
showFun HiFunSerialise      =  "serialise"
showFun HiFunDeserialise    =  "deserialise"
showFun HiFunRead           = "read"
showFun HiFunWrite          = "write"
showFun HiFunMkDir          = "mkdir"
showFun HiFunChDir          = "cd"
showFun HiFunParseTime      = "parse-time"
showFun HiFunRand           = "rand"
showFun HiFunEcho           = "echo"
showFun HiFunCount          = "count"
showFun HiFunKeys           = "keys"
showFun HiFunValues         = "values"
showFun HiFunInvert         = "invert"

-- | Make `String` output of action value.
showAction :: HiAction -- ^ `HiAction` argument to output
            -> String  -- ^ `String` return value
showAction (HiActionRead pathTo) = "read" ++ "(" ++ "\"" ++ pathTo ++ "\"" ++ ")"
showAction (HiActionWrite pathTo writeTo) = "write" ++ "(" ++ "\"" ++ pathTo ++ "\"" ++ "," ++ showBytes writeTo ++ ")"
showAction (HiActionMkDir pathTo) = "mkdir" ++ "(" ++ "\"" ++ pathTo ++ "\"" ++ ")"
showAction (HiActionChDir pathTo) = "cd" ++ "(" ++ "\"" ++ pathTo ++ "\"" ++ ")"
showAction HiActionNow = "now"
showAction HiActionCwd = "cwd"
showAction (HiActionRand start end) = "rand" ++ "(" ++ show start ++ "," ++ show end ++ ")"
showAction (HiActionEcho txt) = "echo" ++ "(" ++ "\"" ++ show txt ++ "\"" ++ ")"

-- | Make `String` output of bytes.
showBytes :: ByteString -- ^ `ByteString` argument to output
              -> String -- ^ `String` return value
showBytes bs = "[#" ++ Data.ByteString.foldl (\a c -> a ++ " " ++ printf "%02x" c) "" bs ++ " " ++ "#]"

-- | Make `String` output of dictionary.
showDict :: (HiValue, HiValue) -> [(HiValue, HiValue)] -> String
showDict (x1, x2) [] = " " ++ prettyValueString x1 ++ " " ++ ":" ++ " " ++ prettyValueString x2
showDict (x1, x2) (xsh:xst) = " " ++ prettyValueString x1 ++ ":" ++ " " ++ prettyValueString x2 ++ "," ++ showDict xsh xst
