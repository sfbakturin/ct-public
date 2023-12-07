module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

newtype State s a = S { runS :: s -> Annotated s a }

-- | Main implementation of `mapState` changing only context.
mapState :: (a -> b) -> State s a -> State s b
mapState f left = S (mapAnnotated f . runS left)

-- | Main implementation of `wrapState` creating new empty state.
wrapState :: a -> State s a
wrapState left = S (left :#)

-- | Main implementation of `joinState` returning context as state.
joinState :: State s (State s a) -> State s a
joinState (S left) = S (\s -> let (x :# xs) = left s in runS x xs)

-- | Main implementation of `joinState` changing only stackframe.
modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

-- | Main implementation of `fmap` using as `mapState` as type is required.
instance Functor (State s) where
  fmap = mapState

-- | Main implementation of `pure` and `<*>` using as `wrapState` as type is required and re-opening state to use its function.
instance Applicative (State s) where
  pure = wrapState
  (<*>) (S left) right = S (\s -> let (x :# _) = left s in runS (mapState x right) s)

-- | Main implementation of `bind` using as `joinState` and `fmap` as type is required.
instance Monad (State s) where
  (>>=) left f = joinState (fmap f left)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Eq, Show)

data Expr = Val Double | Op (Prim Expr)
  deriving (Eq, Show)

-- | Main implementation of Numerical `Expr` to build `Expr`.
instance Num Expr where
  (+) left right = Op (Add left right)
  (-) left right = Op (Sub left right)
  (*) left right = Op (Mul left right)
  abs un = Op (Abs un)
  signum un = Op (Sgn un)
  fromInteger left = Val (fromIntegral left)

-- | Main implementation of Fractional `Expr` to build `Expr`.
instance Fractional Expr where
  (/) left right = Op (Div left right)
  fromRational left = Val (fromRational left)

-- | Helper as evaluating function for binary operations.
-- | Constructor - it's one of [Add, Sub, Mul, Div] as binary operator.
-- | Left, Right - arguments.
-- | Math        - main function for changing evaluated last value.
evalBin :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> State [Prim Double] Double
evalBin left right constructor math = do
  evaluatedLeft <- eval left
  evaluatedRight <- eval right
  modifyState (\exprList -> constructor evaluatedLeft evaluatedRight : exprList)
  wrapState (math evaluatedLeft evaluatedRight)

-- | Helper as evaluating function for unary operations.
-- | Constructor - it's one of [Abs, Div] as unary operator.
-- | Unary       - argument.
-- | Math        - main function for changing evaluated last value.
evalUna :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> State [Prim Double] Double
evalUna unary constructor math = do
  evaluatedUnary <- eval unary
  modifyState (\exprList -> constructor evaluatedUnary : exprList)
  wrapState (math evaluatedUnary)

-- | Main implementation of `eval` using helpers to prevent copy-paste.
eval :: Expr -> State [Prim Double] Double
eval (Val cnst)            = wrapState cnst
eval (Op (Add left right)) = evalBin left right Add (+)
eval (Op (Sub left right)) = evalBin left right Sub (-)
eval (Op (Mul left right)) = evalBin left right Mul (*)
eval (Op (Div left right)) = evalBin left right Div (/)
eval (Op (Abs unary))      = evalUna unary Abs abs
eval (Op (Sgn unary))      = evalUna unary Sgn signum
