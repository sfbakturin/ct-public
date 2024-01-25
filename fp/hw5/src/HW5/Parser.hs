module HW5.Parser
  ( parse
  ) where

import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)

import Control.Applicative (optional)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR), makeExprParser)
import Data.ByteString (ByteString, pack)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Text (Text, pack)
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), Parsec, between, choice, many,
                        manyTill, oneOf, runParser, satisfy, sepBy, sepBy1, skipMany, (<|>))
import Text.Megaparsec.Char (char, spaceChar, string)
import Text.Megaparsec.Char.Lexer (charLiteral, scientific, signed)

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

type HiParser = Parsec Void String

-- | Parse `String` to `HiExpr`, if no error occurred.
-- If no error occurred, then `HiExpr` value
-- Otherwise, its `ParserErrorBundle String Void`
parse :: String                                       -- ^ `String` argument of expression to parse into `HiExpr`
      -> Either (ParseErrorBundle String Void) HiExpr -- ^ `Either` return value
parse = runParser (pElement pExpr <* pEndOfLine) ""

-- | Utility parser for skipping all space's characters, if they occurred.
pSkipWhitespace :: HiParser () -- ^ `HiParser ()` return value
pSkipWhitespace = skipMany spaceChar

-- | Utility parser that requires EOF.
pEndOfLine :: HiParser () -- ^ `HiParser ()` return value
pEndOfLine = eof

-- | Utility parser for determining any non-space element in expression.
pElement :: HiParser a  -- ^ `HiParser` argument to select non-space element
          -> HiParser a -- ^ `HiParser` return value
pElement p = (pSkipWhitespace *> p) <* pSkipWhitespace

-- | Parser for common expressions.
pExpr :: HiParser HiExpr -- ^ `HiParser HiExpr` return value
pExpr = pElement (makeExprParser (do
  function <- pElement (tryHiValue <|> tryBracketsExpr <|> tryListExpr <|> tryDictExpr)
  arguments <- many tryArgs
  annotationRUN <- pElement (optional rRUN)
  case annotationRUN of
    Nothing -> do
      annotationDot <- pElement (optional tryKey)
      case annotationDot of
        Nothing -> return (foldl HiExprApply function arguments)
        Just v -> do
          argumentsAfterAnnotation <- many (pElement tryKeys <|> tryArgs)
          return (foldl HiExprApply (foldl HiExprApply function arguments) ([v] : argumentsAfterAnnotation))
    Just _  -> return (HiExprRun (foldl HiExprApply function arguments))
  ) pOperator)

-- | Parser for element of dictionary in expressions.
pDictElement :: HiParser (HiExpr, HiExpr) -- ^ `HiParser (HiExpr, HiExpr)` return value
pDictElement = do
  key <- pElement pExpr
  _ <- pElement rKV
  value <- pElement pExpr
  return (key, value)

-- | Parser for operators in expressions.
pOperator :: [[Operator HiParser HiExpr]] -- ^ `[[Operator HiParser HiExpr]]` return value
pOperator =
  [
    [
      pOperatorBinL "/" '=' HiFunDiv,
      pOperatorBinL "*" '\0' HiFunMul
    ],
    [
      pOperatorBinL "+" '\0' HiFunAdd,
      pOperatorBinL "-" '\0' HiFunSub
    ],
    [
      pOperatorBinN "/=" '\0' HiFunNotEquals,
      pOperatorBinN "==" '\0' HiFunEquals,
      pOperatorBinN "<=" '\0' HiFunNotGreaterThan,
      pOperatorBinN ">=" '\0' HiFunNotLessThan,
      pOperatorBinN "<" '=' HiFunLessThan,
      pOperatorBinN ">" '=' HiFunGreaterThan
    ],
    [
      pOperatorBinR "&&" '\0' HiFunAnd
    ],
    [
      pOperatorBinR "||" '\0' HiFunOr
    ]
  ]

-- | Helper parser for infix-right binary operators expressions.
pOperatorBinR :: String                   -- ^ `String` argument expecting follow by for operator
              -> Char                     -- ^ `Char` argument non-expecting as followed by expected `String`
              -> HiFun                    -- ^ `HiFun` argument for constructing `HiExpr` of parsed operator
              -> Operator HiParser HiExpr -- ^ `Operator HiParser HiExpr` return value
pOperatorBinR = pOperatorBinAssoc InfixR

-- | Helper parser for non-infix binary operators expressions.
pOperatorBinN :: String                   -- ^ `String` argument expecting follow by for operator
              -> Char                     -- ^ `Char` argument non-expecting as followed by expected `String`
              -> HiFun                    -- ^ `HiFun` argument for constructing `HiExpr` of parsed operator
              -> Operator HiParser HiExpr -- ^ `Operator HiParser HiExpr` return value
pOperatorBinN = pOperatorBinAssoc InfixN

-- | Helper parser for infix-left binary operators expressions.
pOperatorBinL :: String                   -- ^ `String` argument expecting follow by for operator
              -> Char                     -- ^ `Char` argument non-expecting as followed by expected `String`
              -> HiFun                    -- ^ `HiFun` argument for constructing `HiExpr` of parsed operator
              -> Operator HiParser HiExpr -- ^ `Operator HiParser HiExpr` return value
pOperatorBinL = pOperatorBinAssoc InfixL

-- | Helper parser for all infix or non-infix binary operators expressions.
pOperatorBinAssoc :: (HiParser (HiExpr -> HiExpr -> HiExpr) -> Operator HiParser HiExpr) -- ^ `(HiParser (HiExpr -> HiExpr -> HiExpr) -> Operator HiParser HiExpr)` argument for associating operator as non, left or right infix
                  -> String                                                              -- ^ `String` argument expecting follow by for operator
                  -> Char                                                                -- ^ `Char` argument non-expecting as followed by expected `String`
                  -> HiFun                                                               -- ^ `HiFun` argument for constructing `HiExpr` of parsed operator
                  -> Operator HiParser HiExpr                                            -- ^ `Operator HiParser HiExpr` return value
pOperatorBinAssoc assoc expected notFollow ctor = assoc (pOperatorBinRaw expected notFollow ctor)

-- | Helper parser for raw binary operators expressions.
pOperatorBinRaw :: String                                -- ^ `String` argument expecting follow by for operator
                -> Char                                  -- ^ `Char` argument non-expecting as followed by expected `String`
                -> HiFun                                 -- ^ `HiFun` argument for constructing `HiExpr` of parsed operator
                -> HiParser (HiExpr -> HiExpr -> HiExpr) -- ^ `HiParser (HiExpr -> HiExpr -> HiExpr)` return value
pOperatorBinRaw expected notFollow ctor = do
  _ <- try (do { _ <- string expected
  ;
  notFollowedBy (rChar notFollow)
  })
  return (\l r -> HiExprApply (HiExprValue (HiValueFunction ctor)) [l, r])

-- | Try-harder parser for parsing arguments for function in expression.
tryArgs :: HiParser [HiExpr] -- ^ `HiParser [HiExpr]` return value
tryArgs = pElement (fmtBrackets (sepBy pExpr rComma))

-- | Try-harder parser for parsing dot arguments as key for dictionary in expression.
tryKeys :: HiParser [HiExpr] -- ^ `HiParser [HiExpr]` return value
tryKeys = do
  _ <- pElement rDot
  keys <-  tryAnnotatedString
  let key = intercalate "-" keys
  return [(HiExprValue (HiValueString (Data.Text.pack key)))]

-- | Try-harder parser for dot single argument as key for dictionary in expression.
tryKey :: HiParser HiExpr -- ^ `HiParser [HiExpr]` return value
tryKey = do
  _ <- pElement rDot
  keys <-  tryAnnotatedString
  let key = intercalate "-" keys
  return (HiExprValue (HiValueString (Data.Text.pack key)))

-- | Try-harder parser for parsing expression in brackets.
tryBracketsExpr :: HiParser HiExpr -- ^ `HiParser HiExpr` return value
tryBracketsExpr = pElement (fmtBrackets pExpr)

-- | Try-harder parser for parsing list value in expression.
tryListExpr :: HiParser HiExpr -- ^ `HiParser HiExpr` return value
tryListExpr = HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> pElement (fmtList (sepBy pExpr rComma))

-- | Try-harder parser for parsing dict value in expression.
tryDictExpr :: HiParser HiExpr -- ^ `HiParser HiExpr` return value
tryDictExpr = HiExprDict <$> pElement (fmtDict (sepBy pDictElement rComma))

-- | Formatter parser for requiring "(" and ")" between parsing expression.
fmtBrackets :: HiParser a -- ^ `HiParser` argument that required being between "(" and ")"
            -> HiParser a -- ^ `HiParser` return value
fmtBrackets = between rLB rRB

-- | Formatter parser for requiring "[" and "]" between parsing expression.
fmtList :: HiParser a -- ^ `HiParser` argument that required being between "[" and "]"
        -> HiParser a -- ^ `HiParser` return value
fmtList = between rSLB rSRB

-- | Formatter parser for requiring "[#" and "#]" between parsing expression.
fmtBytes :: HiParser a  -- ^ `HiParser` argument that required being between "[#" and "#]"
          -> HiParser a -- ^ `HiParser` return value
fmtBytes = between rSLBL rSRBL

-- | Formatter parser for requiring "{" and "}" between parsing expression.
fmtDict :: HiParser a  -- ^ `HiParser` argument that required being between "{" and "}"
          -> HiParser a -- ^ `HiParser` return value
fmtDict = between rLBF rRBF

-- | Helper parser for requiring any character to parse follow by.
rChar :: Char          -- ^ `Char` argument that required follow by
      -> HiParser Char -- ^ `HiParser Char` return value
rChar = char

-- | Helper parser for requiring any string to parse follow by.
rString :: String          -- ^ `String` argument that required follow by
        -> HiParser String -- ^ `HiParser String` return value
rString = string

-- | Helper parser for requiring "[#" to parse follow by.
rSLBL :: HiParser String -- ^ `HiParser String` return value
rSLBL = rString "[#"

-- | Helper parser for requiring "#]" to parse follow by.
rSRBL :: HiParser String -- ^ `HiParser String` return value
rSRBL = rString "#]"

-- | Helper parser for requiring "[" to parse follow by.
rSLB :: HiParser Char -- ^ `HiParser Char` return value
rSLB = rChar '['

-- | Helper parser for requiring "]" to parse follow by.
rSRB :: HiParser Char -- ^ `HiParser Char` return value
rSRB = rChar ']'

-- | Helper parser for requiring "(" to parse follow by.
rLB :: HiParser Char -- ^ `HiParser Char` return value
rLB = rChar '('

-- | Helper parser for requiring ")" to parse follow by.
rRB :: HiParser Char -- ^ `HiParser Char` return value
rRB = rChar ')'

-- | Helper parser for requiring "{" to parse follow by.
rLBF :: HiParser Char -- ^ `HiParser Char` return value
rLBF = rChar '{'

-- | Helper parser for requiring "}" to parse follow by.
rRBF :: HiParser Char -- ^ `HiParser Char` return value
rRBF = rChar '}'

-- | Helper parser for requiring "," to parse follow by.
rComma :: HiParser Char -- ^ `HiParser Char` return value
rComma = rChar ','

-- | Helper parser for requiring ":" to parse follow by.
rKV :: HiParser Char -- ^ `HiParser Char` return value
rKV = rChar ':'

-- | Helper parser for requiring "!" to parse follow by.
rRUN :: HiParser Char -- ^ `HiParser Char` return value
rRUN = rChar '!'

-- | Helper parer for requiring "." to parse follow by.
rDot :: HiParser Char -- ^ `HiParser Char` return value
rDot = rChar '.'

-- | Helper parser for requiring "-" to parse follow by.
rDash :: HiParser Char -- ^ `HiParser Char` return value
rDash = rChar '-'

-- | Helper parser for requiring any hex character to parse follow by.
rHex :: HiParser Char -- ^ `HiParser Char` return value
rHex = oneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F']

-- | Helper parser constructor for most common values in expression.
ctorTryHiValue :: (a -> HiValue)    -- ^ `(a -> HiValue)` argument to construct success-parsed value
                -> HiParser a       -- ^ `HiParser` argument for parsing value
                -> HiParser HiValue -- ^ `HiParser HiValue` return value
ctorTryHiValue ctor atom = pElement (ctor <$> pElement atom)

-- | Big try-harder parser for parsing most common values in expression.
tryHiValue :: HiParser HiExpr -- ^ `HiParser HiExpr` return value
tryHiValue = HiExprValue <$> choice
  [
    tryHiValueNumber,
    tryHiValueFunction,
    tryHiValueBool,
    tryHiValueString,
    tryHiValueBytes,
    tryHiValueNull,
    tryHiValueCwd,
    tryHiValueNow
  ]

-- | Try-harder parser for parsing `HiActionCwd` in expression.
tryHiValueCwd :: HiParser HiValue
tryHiValueCwd = pElement ((HiValueAction HiActionCwd) <$ string "cwd")

-- | Try-harder parser for parsing `HiActionCwd` in expression.
tryHiValueNow :: HiParser HiValue
tryHiValueNow = pElement ((HiValueAction HiActionNow) <$ string "now")

-- | Constructed try-harder for parsing `HiValueBytes` in expression.
tryHiValueBytes :: HiParser HiValue -- ^ `HiParser HiValue` return value
tryHiValueBytes = ctorTryHiValue HiValueBytes tryBytes

-- | Constructed try-harder for parsing `HiValueNumber` in expression.
tryHiValueNumber :: HiParser HiValue -- ^ `HiParser HiValue` return value
tryHiValueNumber = ctorTryHiValue HiValueNumber tryNumber

-- | Constructed try-harder for parsing `HiValueFunction` in expression.
tryHiValueFunction :: HiParser HiValue -- ^ `HiParser HiValue` return value
tryHiValueFunction = ctorTryHiValue HiValueFunction tryFunction

-- | Constructed try-harder for parsing `HiValueBool` in expression.
tryHiValueBool :: HiParser HiValue -- ^ `HiParser HiValue` return value
tryHiValueBool = ctorTryHiValue HiValueBool tryBool

-- | Constructed try-harder for parsing `HiValueString` in expression.
tryHiValueString :: HiParser HiValue -- ^ `HiParser HiValue` return value
tryHiValueString = ctorTryHiValue HiValueString tryString

-- | Try-harder parser for parsing `HiValueNull` in expression.
tryHiValueNull :: HiParser HiValue -- ^ `HiParser HiValue` return value
tryHiValueNull = pElement (HiValueNull <$ string "null")

-- | Helper parser for `Rational` rational.
tryNumber :: HiParser Rational -- ^ `HiParser Rational` return value
tryNumber = toRational <$> signed pSkipWhitespace scientific

-- | Helper parser for `ByteString` value.
tryBytes :: HiParser ByteString -- ^ `HiParser ByteString` return value
tryBytes = Data.ByteString.pack <$> pElement (fmtBytes (pElement (many (pElement tryByte))))

-- | Helper function for converting `Word8` from hex characters.
charsToW8 :: Char  -- ^ `Char` argument of left bits
          -> Char  -- ^ `Char` argument of right bits
          -> Word8 -- ^ `Word8` return value
charsToW8 a b = read ['0', 'x', a, b]

-- | Helper parser for `Word8` value.
tryByte :: HiParser Word8 -- ^ `HiParser Word8` return value
tryByte = do
  l4 <- rHex
  r4 <- rHex
  return (charsToW8 l4 r4)

-- | Helper parser for `HiFun` value.
tryFunction :: HiParser HiFun -- ^ `HiParser HiFun` return value
tryFunction = choice
  [
    HiFunAdd <$ string "add",
    HiFunSub <$ string "sub",
    HiFunMul <$ string "mul",
    HiFunDiv <$ string "div",
    HiFunNotLessThan <$ string "not-less-than",
    HiFunNotGreaterThan <$ string "not-greater-than",
    HiFunNotEquals <$ string "not-equals",
    HiFunNot <$ string "not",
    HiFunAnd <$ string "and",
    HiFunOr <$ string "or",
    HiFunLessThan <$ string "less-than",
    HiFunGreaterThan <$ string "greater-than",
    HiFunEquals <$ string "equals",
    HiFunIf <$ string "if",
    HiFunLength <$ string "length",
    HiFunToUpper <$ string "to-upper",
    HiFunToLower <$ string "to-lower",
    HiFunReverse <$ string "reverse",
    HiFunTrim <$ string "trim",
    HiFunList <$ string "list",
    HiFunRange <$ string "range",
    HiFunFold <$ string "fold",
    HiFunPackBytes <$ string "pack-bytes",
    HiFunUnpackBytes <$ string "unpack-bytes",
    HiFunEncodeUtf8 <$ string "encode-utf8",
    HiFunDecodeUtf8 <$ string "decode-utf8",
    HiFunZip <$ string "zip",
    HiFunUnzip <$ string "unzip",
    HiFunSerialise <$ string "serialise",
    HiFunDeserialise <$ string "deserialise",
    HiFunRead <$ string "read",
    HiFunWrite <$ string "write",
    HiFunMkDir <$ string "mkdir",
    HiFunChDir <$ string "cd",
    HiFunParseTime <$ string "parse-time",
    HiFunRand <$ string "rand",
    HiFunEcho <$ string "echo",
    HiFunCount <$ string "count",
    HiFunKeys <$ string "keys",
    HiFunValues <$ string "values",
    HiFunInvert <$ string "invert"
  ]

-- | Helper parser for `Bool` value.
tryBool :: HiParser Bool -- ^ `HiParser Bool` return value
tryBool = choice
  [
    True <$ string "true",
    False <$ string "false"
  ]

-- | Helper parser for `Text` value.
tryString :: HiParser Text -- ^ `HiParser Text` return value
tryString = Data.Text.pack <$> (rChar '"' >> manyTill charLiteral (char '"'))

-- | Helper parser for special strings as keys for dictionary.
tryAnnotatedString :: HiParser [[Char]] -- ^ `HiParser [[Char]]` return value
tryAnnotatedString = (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` rDash)
