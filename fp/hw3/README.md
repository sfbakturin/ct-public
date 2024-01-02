# Домашнее задание №3

## Задание 1

1. Create a module named `HW3.T1` and define the following data types in it:

    ```haskell
    data Option a = None | Some a

    data Pair a = P a a

    data Quad a = Q a a a a

    data Annotated e a = a :# e
    infix 0 :#

    data Except e a = Error e | Success a

    data Prioritised a = Low a | Medium a | High a

    data Stream a = a :> Stream a
    infixr 5 :>

    data List a = Nil | a :. List a
    infixr 5 :.

    data Fun i a = F (i -> a)

    data Tree a = Leaf | Branch (Tree a) a (Tree a)
    ```

2. For each of those types, implement a function of the following form:

    ```haskell
    mapF :: (a -> b) -> (F a -> F b)
    ```

    That is, implement the following functions:

    ```haskell
    mapOption      :: (a -> b) -> (Option a -> Option b)
    mapPair        :: (a -> b) -> (Pair a -> Pair b)
    mapQuad        :: (a -> b) -> (Quad a -> Quad b)
    mapAnnotated   :: (a -> b) -> (Annotated e a -> Annotated e b)
    mapExcept      :: (a -> b) -> (Except e a -> Except e b)
    mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
    mapStream      :: (a -> b) -> (Stream a -> Stream b)
    mapList        :: (a -> b) -> (List a -> List b)
    mapFun         :: (a -> b) -> (Fun i a -> Fun i b)
    mapTree        :: (a -> b) -> (Tree a -> Tree b)
    ```

    These functions must modify only the elements and preserve the overall structure (e.g. do not reverse the list, do not rebalance the tree, do not swap the pair).

    This property is witnessed by the following laws:

    ```haskell
             mapF id  ≡  id
    mapF f ∘ mapF g   ≡  mapF (f ∘ g)
    ```

    You must implement these functions by hand, without using any predefined functions (not even from `Prelude`) or deriving.

## Задание 2

Create a module named `HW3.T2`. For each type from the first task except `Tree`, implement functions of the following form:

  ```haskell
  distF :: (F a, F b) -> F (a, b)
  wrapF :: a -> F a
  ```

That is, implement the following functions:

  ```haskell
  distOption      :: (Option a, Option b) -> Option (a, b)
  distPair        :: (Pair a, Pair b) -> Pair (a, b)
  distQuad        :: (Quad a, Quad b) -> Quad (a, b)
  distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
  distExcept      :: (Except e a, Except e b) -> Except e (a, b)
  distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
  distStream      :: (Stream a, Stream b) -> Stream (a, b)
  distList        :: (List a, List b) -> List (a, b)
  distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
  ```

  ```haskell
  wrapOption      :: a -> Option a
  wrapPair        :: a -> Pair a
  wrapQuad        :: a -> Quad a
  wrapAnnotated   :: Monoid e => a -> Annotated e a
  wrapExcept      :: a -> Except e a
  wrapPrioritised :: a -> Prioritised a
  wrapStream      :: a -> Stream a
  wrapList        :: a -> List a
  wrapFun         :: a -> Fun i a
  ```

The following laws must hold:

* Homomorphism:

  ```haskell
  distF (wrapF a, wrapF b)  ≅  wrapF (a, b)
  ```

* Associativity:

  ```haskell
  distF (p, distF (q, r))   ≅  distF (distF (p, q), r)
  ```

* Left and right identity:

  ```haskell
  distF (wrapF (), q)  ≅  q
  distF (p, wrapF ())  ≅  p
  ```

In the laws stated above, we reason up to the following isomorphisms:

  ```haskell
  ((a, b), c)  ≅  (a, (b, c))  -- for associativity
      ((), b)  ≅  b            -- for left identity
      (a, ())  ≅  a            -- for right identity
  ```

There is more than one way to implement some of these functions. In addition to the laws, take the following expectations into account:

* `distPrioritised` must pick the higher priority out of the two.
* `distList` must associate each element of the first list with each element of the second list (i.e. the resulting list is of length `n × m`).

You must implement these functions by hand, using only:

* data types and functions that you defined in `HW3.T1`
* `(<>)` and `mempty` for `Annotated`

## Задание 3

Create a module named `HW3.T3`. For `Option`, `Except`, `Annotated`, `List`, and `Fun` define a function of the following form:

  ```haskell
  joinF :: F (F a) -> F a
  ```

That is, implement the following functions:

  ```haskell
  joinOption    :: Option (Option a) -> Option a
  joinExcept    :: Except e (Except e a) -> Except e a
  joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
  joinList      :: List (List a) -> List a
  joinFun       :: Fun i (Fun i a) -> Fun i a
  ```

The following laws must hold:

* Associativity:

  ```haskell
  joinF (mapF joinF m)  ≡  joinF (joinF m)
  ```

  In other words, given `F (F (F a))`, it does not matter whether we join the outer layers or the inner layers first.

* Left and right identity:

  ```haskell
  joinF      (wrapF m)  ≡  m
  joinF (mapF wrapF m)  ≡  m
  ```

  In other words, layers created by `wrapF` are identity elements to `joinF`.

  Given `F a`, you can add layers outside/inside to get `F (F a)`, but `joinF` flattens it back into `F a` without any other changes to the structure.

Furthermore, `joinF` is strictly more powerful than `distF` and can be used to define it:

  ```haskell
  distF (p, q) = joinF (mapF (\a -> mapF (\b -> (a, b)) q) p)
  ```

At the same time, this is only one of the possible `distF` definitions (e.g. `List` admits at least two lawful `distF`). It is common in Haskell to expect `distF` and `joinF` to agree in behavior, so the above equation must hold.  (Do not redefine `distF` using `joinF`, though: it would be correct but not the point of the exercise).

## Задание 4

1. Create a module named `HW3.T4` and define the following data type in it:

    ```haskell
    data State s a = S { runS :: s -> Annotated s a }
    ```

2. Implement the following functions:

    ```haskell
    mapState :: (a -> b) -> State s a -> State s b
    wrapState :: a -> State s a
    joinState :: State s (State s a) -> State s a
    modifyState :: (s -> s) -> State s ()
    ```

    Using those functions, define `Functor`, `Applicative`, and `Monad` instances for `State s`.

    These instances will enable the use of `do`-notation with `State`.

    The semantics of `State` are such that the following holds:

    ```haskell
    runS (do modifyState f; modifyState g; return a) x
      ≡
    a :# g (f x)
    ```

    In other words, we execute stateful actions left-to-right, passing the state from one to another.

3. Define the following data type, representing a small language:

    ```haskell
    data Prim a =
        Add a a      -- (+)
      | Sub a a      -- (-)
      | Mul a a      -- (*)
      | Div a a      -- (/)
      | Abs a        -- abs
      | Sgn a        -- signum

    data Expr = Val Double | Op (Prim Expr)
    ```

    For notational convenience, define the following instances:

    ```haskell
    instance Num Expr where
      x + y = Op (Add x y)
      x * y = Op (Mul x y)
      ...
      fromInteger x = Val (fromInteger x)

    instance Fractional Expr where
      ...
    ```

    So that `(3.14 + 1.618 :: Expr)` produces this syntax tree:

    ```haskell
    Op (Add (Val 3.14) (Val 1.618))
    ```

4. Using `do`-notation for `State` and combinatorics we defined for it (`pure`, `modifyState`), define the evaluation function:

    ```haskell
    eval :: Expr -> State [Prim Double] Double
    ```

    In addition to the final result of evaluating an expression, it accumulates a trace of all individual operations:

    ```haskell
    runS (eval (2 + 3 * 5 - 7)) []
      ≡
    10 :# [Sub 17 7, Add 2 15, Mul 3 5]
    ```

    The head of the list is the last operation, this way adding another operation to the trace is O(1).

    You can use the trace to observe the evaluation order. Consider this expression:

    ```haskell
    (a * b) + (x * y)
    ```

    In `eval`, we choose to evaluate `(a * b)` first and `(x * y)` second, even though the opposite is also possible and would not affect the final result of the computation.
