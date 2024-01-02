# Домашнее задание №0

## Задание 1

1. Create a module named `HW0.T1` and define the following type in it:

    ```haskell
    data a <-> b = Iso (a -> b) (b -> a)

    flipIso :: (a <-> b) -> (b <-> a)
    flipIso (Iso f g) = Iso g f

    runIso :: (a <-> b) -> (a -> b)
    runIso (Iso f _) = f
    ```

2. Implement the following functions and isomorphisms:

    ```haskell
    distrib :: Either a (b, c) -> (Either a b, Either a c)
    assocPair :: (a, (b, c)) <-> ((a, b), c)
    assocEither :: Either a (Either b c) <-> Either (Either a b) c
    ```

> Note: data `a <-> b = Iso (a -> b) (b -> a)` is the same as `data Iso a b = Iso (a -> b) (b -> a)` but with type name in operator form. It requires `TypeOperators` language extension to be enabled.

## Задание 2

1. Create a module named `HW0.T2` and define the following type in it:

    ```haskell
    type Not a = a -> Void
    ```

2. Implement the following functions and isomorphisms:

    ```haskell
    doubleNeg :: a -> Not (Not a)
    reduceTripleNeg :: Not (Not (Not a)) -> Not a
    ```

## Задание 3

1. Create a module named `HW0.T3` and define the following combinators in it:

    ```haskell
    s :: (a -> b -> c) -> (a -> b) -> (a -> c)
    s f g x = f x (g x)

    k :: a -> b -> a
    k x y = x
    ```

2. Using *only those combinators* and function application (i.e. no lambdas, pattern matching, and so on) define the following additional combinators:

    ```haskell
    i :: a -> a
    compose :: (b -> c) -> (a -> b) -> (a -> c)
    contract :: (a -> a -> b) -> (a -> b)
    permute :: (a -> b -> c) -> (b -> a -> c)
    ```

    For example:

    ```haskell
    i x = x         -- No (parameters on the LHS disallowed)
    i = \x -> x     -- No (lambdas disallowed)
    i = Prelude.id  -- No (only use s and k)
    i = s k k       -- OK
    i = (s k) k     -- OK (parentheses for grouping allowed)
    ```

## Задание 4

1. Create a module named `HW0.T4`.

2. Using the `fix` combinator from the `Data.Function` module define the following functions:

    ```haskell
    repeat' :: a -> [a]             -- behaves like Data.List.repeat
    map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map
    fib :: Natural -> Natural       -- computes the n-th Fibonacci number
    fac :: Natural -> Natural       -- computes the factorial
    ```

    Do not use explicit recursion. For example:

    ```haskell
    repeat' = Data.List.repeat     -- No (obviously)
    repeat' x = x : repeat' x      -- No (explicit recursion disallowed)
    repeat' x = fix (x:)           -- OK
    ```

## Задание 5

1. Create a module named `HW0.T5` and define the following type in it:

    ```haskell
    type Nat a = (a -> a) -> a -> a
    ```

2. Implement the following functions:

    ```haskell
    nz :: Nat a
    ns :: Nat a -> Nat a

    nplus, nmult :: Nat a -> Nat a -> Nat a

    nFromNatural :: Natural -> Nat a
    nToNum :: Num a => Nat a -> a
    ```

3. The following equations must hold:

    ```haskell
    nToNum nz       ==  0
    nToNum (ns x)   ==  1 + nToNum x

    nToNum (nplus a b)   ==   nToNum a + nToNum b
    nToNum (nmult a b)   ==   nToNum a * nToNum b
    ```

## Задание 6

1. Create a module named `HW0.T6` and define the following values in it:

    ```haskell
    a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1
    b = map isSpace "Hello, World"
    c = if 1 > 0 || error "X" then "Y" else "Z"
    ```

2. Determine the WHNF (weak head normal form) of these values:

    ```haskell
    a_whnf = ...
    b_whnf = ...
    c_whnf = ...
    ```
