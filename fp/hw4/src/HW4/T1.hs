module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import qualified Control.Monad

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

-- | Helper as rebuilding only left value from HW3.
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (left :# right) = f left :# right

newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

-- | Main implementation of `mapExceptState` changing only context, if no error.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f left = ES (\s -> case runES left s of
                                    (Success right) -> Success (mapAnnotated f right)
                                    (Error right) -> Error right)

-- | Main implementation of `wrapExceptState` creating new empty successfully state.
wrapExceptState :: a -> ExceptState e s a
wrapExceptState left = ES (\s -> Success (left :# s))

-- | Main implementation of `joinExceptState` returning context as state, if no error.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState left = ES (\s -> case runES left s of
                                        (Success (r :# l)) -> runES r l
                                        (Error right) -> Error right)

-- | Main implementation of `modifyExceptState` changing only stackframe.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

-- | Main implementation of `throwExceptState` creating new error state.
throwExceptState :: e -> ExceptState e s a
throwExceptState left = ES (const (Error left))

-- | Main implementation of `fmap` using as `mapExceptState` as type is required.
instance Functor (ExceptState e s) where
  fmap = mapExceptState

-- | Main implementation of `pure` and `<*>` using as `wrapExceptState` and Control.Monad.ap as type is required and re-opening state to use its function.
instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

-- | Main implementation of `bind` using as `joinExceptState` and `fmap` as type is required.
instance Monad (ExceptState e s) where
  (>>=) left f = joinExceptState (fmap f left)

data EvaluationError = DivideByZero
  deriving Show

-- | Helper as evaluating function for binary operations.
-- | Constructor - it's one of [Add, Sub, Mul, Div] as binary operator.
-- | Left, Right - arguments.
-- | Math        - main function for changing evaluated last value.
evalBin :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
evalBin left right constructor math = do
  evaluatedLeft <- eval left
  evaluatedRight <- eval right
  modifyExceptState (\exprList -> constructor evaluatedLeft evaluatedRight : exprList)
  wrapExceptState (math evaluatedLeft evaluatedRight)

-- | Helper as evaluating function for unary operations.
-- | Constructor - it's one of [Abs, Div] as unary operator.
-- | Unary       - argument.
-- | Math        - main function for changing evaluated last value.
evalUna :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
evalUna unary constructor math = do
  evaluatedUnary <- eval unary
  modifyExceptState (\exprList -> constructor evaluatedUnary : exprList)
  wrapExceptState (math evaluatedUnary)

-- | Helper as evaluating function for safety binary operations.
-- | Constructor - it's one of [Add, Sub, Mul, Div] as binary operator.
-- | Left, Right - arguments.
-- | Math        - main function for changing evaluated last value.
-- | Checker     - checks evaluated left  and right values for invalidness.
evalBinSafety :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> (Double -> Double -> Bool) -> ExceptState EvaluationError [Prim Double] Double
evalBinSafety left right constructor math checker = do
  evaluatedLeft <- eval left
  evaluatedRight <- eval right
  if checker evaluatedLeft evaluatedRight
    then do
      modifyExceptState (\exprList -> constructor evaluatedLeft evaluatedRight : exprList)
      wrapExceptState (math evaluatedLeft evaluatedRight)
    else throwExceptState DivideByZero

-- | Main implementation of `eval` using helpers to prevent copy-paste.
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val cnst)            = wrapExceptState cnst
eval (Op (Add left right)) = evalBin left right Add (+)
eval (Op (Sub left right)) = evalBin left right Sub (-)
eval (Op (Mul left right)) = evalBin left right Mul (*)
eval (Op (Div left right)) = evalBinSafety left right Div (/) (\_ r -> r /= 0)
eval (Op (Abs unary))      = evalUna unary Abs abs
eval (Op (Sgn unary))      = evalUna unary Sgn signum
